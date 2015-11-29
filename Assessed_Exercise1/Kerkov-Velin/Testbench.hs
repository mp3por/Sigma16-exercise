module Main where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import TrafficLight

-- Main program

-- Print a line to separate output from different simulations
separator :: IO ()
separator = putStrLn (take 76 (repeat '-'))

-- The main program runs a sequence of simulations
main :: IO ()
main =
  do
    separator
    putStrLn "Simulate trafficLights controller1"
    run_trafficLights_controller1 trafficLights_controller1_input1

    separator
    putStrLn "Simulate trafficLights controller1"
    run_trafficLights_controller1 trafficLights_controller1_input2

    separator
    putStrLn "Simulate walkCount"
    run_walkCount 16 walkCount_input1

    separator
    putStrLn "Simulate trafficLights controller2 ( if green stays on forever and handles reset correctly )"
    run_trafficLights_controller2 trafficLights_controller2_input1

    separator
    putStrLn "Simulate trafficLights controller2 ( if walkRequest works in the ideal case )"
    run_trafficLights_controller2 trafficLights_controller2_input2

    separator
    putStrLn "Simulate trafficLights controller2 ( if walkRequest works in the ideal case )"
    run_trafficLights_controller2 trafficLights_controller2_input3

    separator
    putStrLn "Simulate trafficLights controller2 ( if walkRequest works with a reset in the end )"
    run_trafficLights_controller2 trafficLights_controller2_input4

    separator
    putStrLn "Simulate trafficLights controller2 ( if walkRequests are counted in the middle of a walkRequest )"
    run_trafficLights_controller2 trafficLights_controller2_input6

-- Simulation driver for Traffic Lights controller2
run_trafficLights_controller2 input = runAllInput input output
  where
    -- Extract input bits
    reset       = getbit input 0
    walkRequest = getbit input 1

    -- Attach circuit
    (green,amber,red,wait,walk,walkCount) = controller2 reset walkRequest

    -- Format output
    output = [string "reset: ", bit reset,
              string ",walkRequest: ", bit walkRequest,
              string " -> green: ", bit green,
              string ", amber: ", bit amber,
              string ", red: ", bit red,
              string ", wait: ", bit wait,
              string ", walk: ", bit walk,
              string ", counter: ", hex walkCount
              ]

-- Simulation driver for Traffic Lights controller1
run_trafficLights_controller1 input = runAllInput input output
  where
    -- Extract input bit
    x = getbit input 0

    -- Attach circuit
    (green,amber,red) = controller1 x

    -- Format output
    output = [string "reset: ", bit x,
              string " -> green: ", bit green,
              string ", amber: ",bit amber,
              string ", red: ",bit red]


-- Simulation driver for walk count
-- Used to test if the circuit performs what it has to do
run_walkCount :: Int -> [[Int]] -> IO ()
run_walkCount n input = runAllInput input output
  where
    -- Extract input bits
    reset       = getbit input 0
    walkRequest = getbit input 1

    -- Attach circuit
    walkCount = walkCountCircuit n reset walkRequest

    -- Format output
    output = [string "walkRequest", hex walkCount]


-- Traffic Lights controller1 test data
-- It consists of a reset input bit
-- This tests basic functionality
trafficLights_controller1_input1 =
  -- reset -- reset -> green, amber, red
  [
    [1], -- 1 -> 0, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 0, 1, 0
    [0], -- 0 -> 0, 0, 1
    [0], -- 0 -> 0, 0, 1
    [0], -- 0 -> 0, 0, 1
    [0], -- 0 -> 0, 0, 1
    [0], -- 0 -> 0, 1, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 0, 1, 0
    [0], -- 0 -> 0, 0, 1
    [0], -- 0 -> 0, 0, 1
    [0]  -- 0 -> 0, 0, 1
  ]

-- This tests reset in the middle of the cycles
trafficLights_controller1_input2 =
  -- reset -- reset -> green, amber, red
  [
    [1], -- 1 -> 0, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [1], -- 0 -> 0, 1, 0 -- reset on amber
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 0, 1, 0
    [0], -- 0 -> 0, 0, 1
    [1], -- 0 -> 0, 0, 1 -- reset on red
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 1, 0, 0
    [0], -- 0 -> 0, 1, 0
    [0], -- 0 -> 0, 0, 1
    [0]  -- 0 -> 0, 0, 1
  ]


-- Traffic Lights controller2 test data
-- It consists of a reset and walkRequest input bit.
-- This tests if green stays on forever and handles reset correctly
trafficLights_controller2_input1 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0 -> green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [1    , 0], -- reset: 1, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0]  -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
  ]

-- This test with if walkRequest works in the ideal case
trafficLights_controller2_input2 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0 -> green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 1], -- reset: 0, walkRequest: 1 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0]  -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
  ]

-- This test with if walkRequest works in the ideal case
trafficLights_controller2_input3 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0 -> green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 1], -- reset: 0, walkRequest: 1 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 1], -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0 -> green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0]  -- reset: 0, walkRequest: 0 -> green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0002
  ]

-- This test with if walkRequest works with a reset in the end
trafficLights_controller2_input4 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0, green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0002
    [1    , 0], -- reset: 1, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0]  -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
  ]

-- This test with if walkRequest works with a reset in the middle
trafficLights_controller2_input5 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0, green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 0, walkCount: 0002
    [1    , 0], -- reset: 1, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0]  -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
  ]

-- This test with if walkRequests are counted in the middle of a walkRequest
trafficLights_controller2_input6 =
  -- reset, walkRequest -- reset, wa;lRequest -> green,amber,red,wait,walk,walkCount
  [
    [1    , 0], -- reset: 1, walkRequest: 0, green: 0, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0000
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0001
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0002
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 1], -- reset: 0, walkRequest: 1, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0002
    [0    , 1], -- reset: 0, walkRequest: 1, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0003
    [0    , 1], -- reset: 0, walkRequest: 1, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0004
    [0    , 0], -- reset: 0, walkRequest: 0, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0005
    [0    , 1], -- reset: 0, walkRequest: 1, green: 1, amber: 0, red: 0, wait: 1, walk: 0, walkCount: 0005
    [0    , 0], -- reset: 0, walkRequest: 0, green: 0, amber: 1, red: 0, wait: 1, walk: 0, walkCount: 0006
    [0    , 0]  -- reset: 0, walkRequest: 0, green: 0, amber: 0, red: 1, wait: 0, walk: 1, walkCount: 0006
  ]


-- walkCount test input data
-- It consists of reset and walkRequest input bits
walkCount_input1 :: [[Int]]
walkCount_input1 =
  -- reset, walkRequest -- walkCount
  [
    [1    , 0], -- 0
    [0    , 0], -- 0
    [0    , 1], -- 0
    [0    , 0], -- 1
    [1    , 0], -- 1
    [0    , 1], -- 0
    [0    , 1], -- 1
    [1    , 1], -- 2
    [0    , 0]  -- 0
  ]