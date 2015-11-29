-- Example Circ1 manually transformed
-- See Circ1main for notes on how to run the example

-- Structural Hydra: the circuit is defined in the form of a low level
-- structural specification.  Normally this would be produced
-- automatically by a transformation function, but it's also possible
-- to write the structural specification by hand, as shown here.

module Circ1xform where

import Box
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Language.Haskell.TH

mycirc1' inputs0 = outputs
  where
--
-- Inputs (cluster 0)
    inputs0pat = Pword [Pname (mkName "a"), Pname (mkName "b"),
                        Pname (mkName "c"),Pname (mkName "d")]
    (inclu0,inputs0') = rename inputs0pat (CluUpInport box 0) inputs0
    [a,b,c,d] = inputs0'
--
-- Equation 0
    p = myinv a
    (cleq0,p') = rename (Pname (mkName "p")) (CluUpEqu box [0]) p

-- Equation 1
--    q = myand2 (myinv b) (mybuf c)
--  Transform to...
--    q = myand2 tmp0 tmp1
--    tmp0 = myinv b
--    tmp1 = myinv c
    (cleq1,q') = rename (pname  "q")
    (cleq10,tmp0) = rename 


    (cleq1,q') = rename (Pname (mkName "q")) (CluUpEqu box [1]) q

-- Equation 2
    r = myinv c
    (cleq2,r') = rename (Pname (mkName "r")) (CluUpEqu box [2]) r

-- Equation 3
    s = myinv d
    (cleq3,s') = rename (Pname (mkName "s")) (CluUpEqu box [3]) s

-- Equation 4  (a partial application, defining a circuit)
    f = myand2 a
    (cleq4,f') = rename (Pname (mkName "f")) (CluUpEqu box [4]) p

--
-- Outputs
    outputs0 = [p',q',r',s']
    outpat = Pword [Pname (mkName "p"), Pname (mkName "q"),
                    Pname (mkName "r"),Pname (mkName "s")]
    (outclu,outputs) = rename outpat (CluUpOutport box) outputs0
--
-- Box
    box = Cbox "mycirc1" [inclu0] [cleq0,cleq1,cleq2,cleq3] outclu

