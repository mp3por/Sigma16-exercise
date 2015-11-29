---------------------------------------------------------------------------
-- Hydra: a functional computer hardware description language
---------------------------------------------------------------------------

This module provides a set of combinational building block circuits.

> module HDL.Hydra.Circuits.Combinational where

> import HDL.Hydra.Core.Signal
> import HDL.Hydra.Core.Pattern


------------------------------------------------------------------------
			    Logic on words
------------------------------------------------------------------------

Word inverter: winv takes a word and inverts each of its bits

> winv :: Signal a => [a] -> [a]
> winv x = map inv x

And/Or over a word: Determine whether there exists a 1 in a word, or
whether all the bits are 0.  A tree fold can do this in log time, but
for simplicity this is just a linear time fold.

> orw, andw :: Signal a => [a] -> a
> orw = foldl or2 zero
> andw = foldl and2 one


Building a constant integer word

Representing a boolean bit as a word: boolword takes a bit x, and pads
it to the left with 0s to form a word.  If the input x is False (0),
the result is the integer 0 (i.e. n 0-bits), and if x is True (1) the
result is the integer 1 (rightmost bit is 1, all others are 0).

> boolword :: Signal a => Int -> a -> [a]
> boolword n x = fanout (n-1) zero ++ [x]


------------------------------------------------------------------------
			     Conditionals
------------------------------------------------------------------------

Multiplexers

> mux1 :: Signal a => a -> a -> a -> a
> mux1 p a b = x
>   where x = or2 (and2 (inv p) a) (and2 p b)

> mux2 :: Signal a => (a,a) -> a -> a -> a -> a -> a
> mux2 (c,d) p q r s =
>   mux1 c  (mux1 d p q)
>           (mux1 d r s)

> mux3 :: Signal a => (a,a,a) -> a -> a -> a -> a -> a-> a -> a -> a -> a
> mux3 (c0,c1,c2) a0 a1 a2 a3 a4 a5 a6 a7 =
>   mux1 c0
>     (mux1 c1
>       (mux1 c2 a0 a1)
>       (mux1 c2 a2 a3))
>     (mux1 c1
>       (mux1 c2 a4 a5)
>       (mux1 c2 a6 a7))

> mux22 :: Signal a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
> mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
>   where x = mux2 (p0,p1) a0 b0 c0 d0
>         y = mux2 (p0,p1) a1 b1 c1 d1

> mux1w :: Signal a => a -> [a] -> [a] -> [a]
> mux1w c x y = map2 (mux1 c) x y

> mux2w cc = map4 (mux2 cc)


Demultiplexers

> demux1 :: Signal a => a -> a -> (a,a)
> demux1 c x = (and2 (inv c) x, and2 c x)

> demux2 :: Signal a => (a,a) -> a -> (a,a,a,a)
> demux2 (c0,c1) x = (y0,y1,y2,y3)
>   where  (p,q) = demux1 c0 x
>          (y0,y1) = demux1 c1 p
>          (y2,y3) = demux1 c1 q

> demux1w :: Signal a => [a] -> a -> [a]
> demux1w [c0] x =
>   let (a0,a1) = demux1 c0 x
>   in [a0,a1]

> demux2w :: Signal a => [a] -> a -> [a]
> demux2w [c0,c1] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux1w [c1] a0
>       w1 = demux1w [c1] a1
>   in w0++w1

> demux3w :: Signal a => [a] -> a -> [a]
> demux3w [c0,c1,c2] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux2w [c1,c2] a0
>       w1 = demux2w [c1,c2] a1
>   in w0++w1

> demux4w :: Signal a => [a] -> a -> [a]
> demux4w [c0,c1,c2,c3] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux3w [c1,c2,c3] a0
>       w1 = demux3w [c1,c2,c3] a1
>   in w0++w1


------------------------------------------------------------------------
			      Arithmetic
------------------------------------------------------------------------

Combinational shifting

Shift a word to the right (shr) or to the left (shl).  In both cases,
this is just a wiring pattern.  A 0 is brought in on one side, and the
bit on the other side is just thrown away.

> shr x = zero : [x!!i | i <- [0..k-2]]
>   where k = length x
> shl x = [x!!i | i <- [1..k-1]] ++ [zero]
>   where k = length x


Bit addition

A half adder adds two bits x and y, and produces a 2-bit result
(carry,sum).  The value of x+y can be any of 0, 1, or 2, and this is
the binary value of the 2-bit result.

> halfAdd :: Signal a => a -> a -> (a,a)
> halfAdd x y = (and2 x y, xor2 x y)

A full adder adds three bits x, y, and c, and produces a 2-bit result
(carry,sum).  This is a building block for circuits that add words,
where a carry input c must be added to the x and y bits.  The value of
x+y+c can be any of 0, 1, 2, or 3, and this is the binary value of the
2-bit result.  It is convenient to group the x and y bits into a pair,
and to treat the carry input c as a separate input; this makes it
easier to use fullAdd with the mscanr combinator.

> fullAdd :: Signal a => (a,a) -> a -> (a,a)
> fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)

The carry and sum results from adding x+y+c are calculated by bsum and
bcarry.  For most purposes it would be more concise just to define the
fullAdd circuit directly, but for fast adders it is helpful to
organize the full adder using separate bcarry and bsum subcircuits.
The bit sum is a parity calculation, while the carry is a majority.

> bsum, bcarry :: Signal a => (a,a) -> a -> a
> bsum (x,y) c = xor3 x y c
> bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)


Ripple carry addition

> rippleAdd :: Signal a => a -> [(a,a)] -> (a,[a])
> rippleAdd = mscanr fullAdd


Addition and subtraction

> addSub :: Signal a => a -> [(a,a)] -> (a,[a])
> addSub sub xy = rippleAdd sub (map f xy)
>   where f (x,y) = (x, xor2 sub y)


Binary comparison

Ripple comparison

> rippleCmp :: Signal a => [(a,a)] -> (a,a,a)
> rippleCmp = foldl cmp1 (zero,one,zero)

Building block for comparing bits

> cmp1 :: Signal a => (a,a,a) -> (a,a) -> (a,a,a)
> cmp1 (lt,eq,gt) (x,y) =
>   (or2 lt (and3 eq (inv x) y),
>    and2 eq (inv (xor2 x y)),
>    or2 gt (and3 eq x (inv y))
>   )


------------------------------------------------------------------------
			   Wiring patterns
------------------------------------------------------------------------


Unbuffered Fanout

> fanout2 :: a -> (a,a)
> fanout2 x = (x,x)

> fanout3 :: a -> (a,a,a)
> fanout3 x = (x,x,x)

> fanout4 :: a -> (a,a,a,a)
> fanout4 x = (x,x,x,x)

Duplicating a bit to form a word: fanout takes a wordsize k and a
signal x, and produces a word of size k each of whose bits takes the
value of x.

> fanout :: Signal a => Int -> a -> [a]
> fanout k x = take k (repeat x)


Buffered Fanout

> fanoutbuf2 :: Signal a => a -> (a,a)
> fanoutbuf2 x = (y,y)
>   where y = buf x

> fanoutbuf3 :: Signal a => a -> (a,a,a)
> fanoutbuf3 x = (y,y,y)
>   where y = buf x

> fanoutbuf4 :: Signal a => a -> (a,a,a,a)
> fanoutbuf4 x = (y,y,y,y)
>   where y = buf x

