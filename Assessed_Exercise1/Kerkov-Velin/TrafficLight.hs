module TrafficLights where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

-- inputs : reset signal
-- outputs: (green,amber,red) signals 
controller1 :: Clocked a => a -> (a,a,a)
controller1 reset = (green,amber,red)
  where
    -- -------------------------------------------------------------------------
    -- This circuit has 9 states which are in the end combined to produce the 
    -- end result. Stages |g1,g2,g3,|a1,a2,|r1,r2,r3,r4|
    --                      green     amber     red
    -- Initial state: g1 
    -- Every state depends on the previous and respects the "reset" signal
    -- -------------------------------------------------------------------------

    g1 = dff (or2 reset a2)
    g2 = dff (mux1 reset g1 zero)
    g3 = dff (mux1 reset g2 zero)
    a1 = dff (mux1 reset g3 zero)
    r1 = dff (mux1 reset a1 zero)
    r2 = dff (mux1 reset r1 zero)
    r3 = dff (mux1 reset r2 zero)
    r4 = dff (mux1 reset r3 zero)
    a2 = dff (mux1 reset r4 zero)

    green = or3 g1 g2 g3
    amber = or2 a1 a2
    red = or2 (or2 r1 r2) (or2 r3 r4)


-- inputs : reset, walkRequest signal
-- outputs: (green,amber,red,wait,walk,walkCount) signals 
controller2 :: Clocked a => a -> a -> (a,a,a,a,a,[a])
controller2 reset walkRequest = (green,amber,red,wait,walk,walkCount)
  where
    -- attach the inputs to the walkCount circuit
    n = 16
    walkCount = walkCountCircuit n reset walkRequest

    -- walkRequest must be handled only if we are in state 'green'
    walkRequest' =  and2 walkRequest green
    g1 = dff (mux2 (walkRequest',reset) (or3 a2 zero g1) one zero one)
    a1 = dff (mux2 (walkRequest',reset) zero zero g1 zero)
    r1 = dff (mux1 reset a1 zero)
    r2 = dff (mux1 reset r1 zero)
    r3 = dff (mux1 reset r2 zero)
    a2 = dff (mux1 reset r3 zero)

    walk = red -- walk is '1' only when red is '1'
    wait = inv walk -- wait is the opposite of walk

    -- combine all states 
    red = or3 r1 r2 r3
    amber = or2 a1 a2
    green = g1

----------------------------------------------------
-- 16 bit walk counter
--  _________________________
-- |walkreq|reset| output    |
-- |_______|_____|___________|
-- |   0   |  0  |walkCount  |
-- |   0   |  1  |  0000     | 
-- |   1   |  0  |walkCount+1| 
-- |   1   |  1  |  0000     |
-- |_______|_____|___________|

-- outputs: the current value of the walkCounter
----------------------------------------------------
walkCountCircuit :: Clocked a => Int -> a -> a -> [a]
walkCountCircuit n reset walkRequest = walkCount
  where
    -- pre-processing 
    wzero = fanout n zero
    wone = boolword n one
    (cout,walkCountPlusOne) = rippleAdd zero (bitslice2 walkCount wone)

    -- setting up the load depending on the inputs
    walkCountLoad = mux2w (walkRequest,reset) walkCount wzero walkCountPlusOne wzero

    -- setting up the load_ctr depending on the input
    walkCount_ld_ctr = or2 reset walkRequest

-- The result is whatever the registries output at the moment
    walkCount = reg n walkCount_ld_ctr walkCountLoad