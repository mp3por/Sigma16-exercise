module HDL.Hydra.Examples.Tutorial1 where


import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

runTestbench :: IO ()
runTestbench = sim input1

-- A testbench

sim :: [[Int]] -> IO ()
sim input =
  do putStrLn "\nsim -- demonstrate testbench"
     runAllInput input output
  where
    a = getbit   input 0
    b = getbin 8 input 1
    output =
      [string "hello",
       string " a=", bit a,
       string " b=", hex b,
       fmtIf a
         [string " YES!"]
         [string " NO!"]
      ]

-- Suitable input data

input1 =
  [[0,  35],
   [0,   9],
   [1,  18],
   [0,  27],
   [1,  51],
   [0,  17]]
