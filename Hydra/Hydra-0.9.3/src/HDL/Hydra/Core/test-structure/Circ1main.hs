{-# LANGUAGE TemplateHaskell #-}

-- Example Circ1 main module

-- How to run the example.

-- There are three modules comprising this circuit: Circ1src.hs
-- contains the Hydra source specification, Circ1xform.hs shows the
-- transformed structural representation written out manually, and
-- this module is the main program which runs the circuit.

module Main where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Language.Haskell.TH

import StrucCircuits
import Box
import Circ1struc


main :: IO ()
main =
  do putStrLn "Running Circ1main -- basic specification"
--     run4 mycirc1 test_data_1
--     putStrLn "Running mycirc1' -- structural specification"
--     runmycirc1 test_data_1
--     runStrucmycirc1 test_data_1

------------------------------------------------------------------------     
{- Traversing the circuit structure -}

{-
runStrucmycirc1 :: [[Int]] -> IO ()
runStrucmycirc1 input =
  do putStrLn "\nrunStrucmycirc1"
     traverse [w,x,y,z]
  where
    a = getbit input 0
    b = getbit input 1
    c = getbit input 2
    d = getbit input 3
    a' = StrucSigBase a :: StrucSig (Stream Bool)
    b' = StrucSigBase b
    c' = StrucSigBase c
    d' = StrucSigBase d
    [w,x,y,z] = mycirc1' [a',b',c',d']
-}

------------------------------------------------------------------------     

{- Simulating the structural specification -}

test_data_1 =
  [[0,0,0,0],
   [0,1,1,0],
   [1,1,1,1],
   [1,0,1,0],
   [1,1,1,0],
   [0,0,0,1],
   [0,0,0,0]]

{-
runmycirc1 :: [[Int]] -> IO ()
runmycirc1 input = runAllInput
 input output
  where
    a = getbit input 0
    b = getbit input 1
    c = getbit input 2
    d = getbit input 3
    a' = StrucSigBase a
    b' = StrucSigBase b
    c' = StrucSigBase c
    d' = StrucSigBase d
    [w,x,y,z] = mycirc1' [a',b',c',d']
    w' = getsigval w
    x' = getsigval x
    y' = getsigval y
    z' = getsigval z
    output =
      [string "  Inputs: ",
       bit a, bit b, bit c, bit d,
       string "   Outputs = ",
       bit w', bit x', bit y', bit z']
-}
