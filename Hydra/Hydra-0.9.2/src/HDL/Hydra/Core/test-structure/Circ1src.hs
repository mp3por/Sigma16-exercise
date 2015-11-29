-- See Circ1main.hs for notes on how to run the example.  This file is
-- the user level Hydra specification of some circuits.

{-# LANGUAGE TemplateHaskell #-}

module Circ1src where

import StrucCircuits
import Box
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Language.Haskell.TH

declarations_Circ1src = [d|

 foo a b = (c,d)
   where
     c = and2 (inv a) b
     d = or2 c (inv b)

 |]


{-

 justInvCirc a b = c
   where c = inv a


 bar (p,q,r,s) = (p, and2 q (inv r), s)
   where a = p

 foo x = x

 bar (p,q) [r,s,t] [(w,x),(y,z)] = (p, and2 a b)
   where a = p
         b = q
         c = inv r

 mycirc1 x = x

 bad [] = 1
 bad (x:xs) = 2

 baddef = [0..9]

 mycirc2 [a,b,c] = [p,q]
   where p = myinv a
         q = myand2 (myinv b) (mybuf p)
-}

