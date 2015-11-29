---------------------------------------------------------------------------
-- Hydra hardware description language
-- John O'Donnell, see the README file and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

module HDL.Hydra.Core.SigStream where
import HDL.Hydra.Core.Signal


-- Representation of Streams
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

data Stream a = Cycle a (Stream a)

listStream :: [a] -> Stream a
listStream [] = error "no more stream data from list"
listStream (x:xs) = Cycle x (listStream xs)


input_ints :: [Int] -> Stream Bool
input_ints = listStream . map f
  where f :: Int -> Bool
        f x = if x==0 then False else True



current :: Stream a -> a
current (Cycle x xs) = x

future :: Stream a -> Stream a
future (Cycle x xs) = xs

forever :: a -> Stream a
forever x = Cycle x (forever x)

instance (Show a, Static a) => Show (Stream a) where
  show (Cycle x xs) = showSig x ++ "," ++ show xs


-- Stream Signal Operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~

instance Static a => Signal (Stream a) where
  zero  = forever zero
  one   = forever one
  buf   = mapStream buf
  inv   = mapStream inv
  and2  = map2Stream and2
  and3  = map3Stream and3
  and4  = map4Stream and4
  or2   = map2Stream or2
  or3   = map3Stream or3
  or4   = map4Stream or4
  nand2 = map2Stream nand2
  nand3 = map3Stream nand3
  nand4 = map4Stream nand4
  nor2  = map2Stream nor2
  nor3  = map3Stream nor3
  nor4  = map4Stream nor4
  xor2  = map2Stream xor2
  xor3  = map3Stream xor3
  xor4  = map4Stream xor4
  xnor2 = map2Stream xnor2
  xnor3 = map3Stream xnor3
  xnor4 = map4Stream xnor4
--  alias x _ _ _ = x

mapStream :: (a->a) -> Stream a -> Stream a
mapStream f x
  = Cycle
      (f (current x))
      (mapStream f (future x))

map2Stream :: (a->b->c) -> Stream a -> Stream b -> Stream c
map2Stream f x y =
  Cycle
    (f (current x) (current y))
    (map2Stream f (future x) (future y))

map3Stream :: (a->b->c->d) -> Stream a -> Stream b -> Stream c -> Stream d
map3Stream f x y z =
  Cycle
    (f (current x) (current y) (current z))
    (map3Stream f (future x) (future y) (future z))

map4Stream ::
  (a->b->c->d->e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
map4Stream f w x y z =
  Cycle
    (f (current w) (current x) (current y) (current z))
    (map4Stream f (future w) (future x) (future y) (future z))


-- Streams represent clocked signals
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instance Static a => Clocked (Stream a) where
  dff x = Cycle zero x

intsSig :: Static a => [Int] -> Stream a
intsSig (x:xs) = Cycle (intSig x) (intsSig xs)

-- ..................................................
-- Junk...

{-
  box11 = box11Stream
  box21 = box21Stream
  box31 = box31Stream
  box41 = box41Stream
  box22 = box22Stream
 --  mkOut = mkOut
-}

{- There is something to work out here! ???
box31Stream
  :: (Signal a)
  => String -> String -> String -> String -> String
  -> (Stream a -> Stream a -> Stream a -> Stream a)
  -> Stream a -> Stream a -> Stream a -> Stream a  
-- box31Stream a b c d e f p q r = box31 a b c d e f p q r
    -- the def above  fails!
-- box31Stream a b c d e f p q r = f p q r -- works
box31Stream a b c d e f = f  -- works

-}

{- box11Stream _ _ _ f = f
box21Stream _ _ _ _ f = f
box31Stream _ _ _ _ _ f = f
box41Stream _ _ _ _ _ _ f = f
box22Stream _ _ _ _ _ f = f
-}
