
------------------------------------------------------------------------
-- More examples
------------------------------------------------------------------------
{-# OPTIONS_GHC -fth #-}

{- To build and run, see Test1Main.hs -}

module Test1 where

import Language.Haskell.TH
import HDL.Hydra.Core.Lib

{- This file could (or should) be derived automatically from a source
file written by the user.  The imports are copied verbatim, and the
user's original specification is copied inside the body of the [d|
... |]. -}

------------------------------------------------------------------------
-- Test expressions

x, y :: Bool
x = True
y = False

test_Exp1 :: Q Exp
test_Exp1 = [| and2 x y |]

circ_defs_rep :: Q [Dec]
circ_defs_rep = [d|
------------------------------------------------------------------------
 

 simplest_circ :: Signal a => a -> a
 simplest_circ x = x

 simple_circ :: Signal a => a -> a
 simple_circ x = inv x

 circ_locals :: Signal a => a -> a -> a -> a
 circ_locals a b c = y
   where x = and2 a b
         y = or2 a x

------------------------------------------------------------------------
 |]



{-
mycirc1 x = y
  where y = inv x

strmycirc1' x = y'
  where y = inv x'
        x' = nameCluster x "x" (Incluster 0) box
        y' = nameCluster y "y" (Equation 0) box
        box = makeBox "circ1"

mymux1 c x y = r
  where r = or2 (and2 (inv c) x) (c y)

strmymux1 c x y = r
  where box = Box
        r = alias 

strcirc2 x y = (c,s,z)
  where
    z = or2 c s
    (c,s) = halfAdd x y

strcirc2' x y = (c', s', z')
  where
    (c,s) = halfadd x' y'
    c_clu = CluSgl (Just "c")
             (CluHigherCluster clu_lhs_0 (CluDownTuple 0)) c'
    c' = StrucSig c (
    lhs1Tup = CluTup Nothing
                (CluEqu box 0)
                [CluSgl (Just "c"), CluSgl (Just "s")]
                
    c = gettup 2 0 lhs1
    s = gettup 2 1 lhs1
    lhs2 = or2 c s
    z = alias Sgl lhs2
-}
