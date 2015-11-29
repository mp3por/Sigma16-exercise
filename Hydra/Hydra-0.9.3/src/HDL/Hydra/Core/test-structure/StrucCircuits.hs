module StrucCircuits where

import Box
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Language.Haskell.TH

------------------------------------------------------------------------
-- Primitive structural circuits
------------------------------------------------------------------------

myinv, mybuf :: Signal a => StrucSig a -> StrucSig a

myinv = mkStrucFcn11 "myinv" inv
mybuf = mkStrucFcn11 "mybuf" buf

myand2, mynand2, myor2, mynor2, myxor2, myxnor2
  :: Signal a => StrucSig a -> StrucSig a -> StrucSig a

myand2  = mkStrucFcn21 "myand2"  and2
mynand2 = mkStrucFcn21 "mynand2" nand2
myor2   = mkStrucFcn21 "myor2"   or2
mynor2  = mkStrucFcn21 "mynor2"  nor2
myxor2  = mkStrucFcn21 "myxor2"  xor2
myxnor2 = mkStrucFcn21 "myxnor2" xnor2
