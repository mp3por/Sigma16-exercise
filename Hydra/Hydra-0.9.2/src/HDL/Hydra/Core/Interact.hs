
module HDL.Hydra.Core.Interact where

import Data.IORef

data Node1 = Node1 Int (IORef Node1)

data Node2 = Node2 (IORef Int) (IORef Node2)

printlist :: IORef Node1 -> IO ()
printlist r = do
  Node1 i n <- readIORef r
  putStrLn (show i)
  printlist n

------------------------------------------------------------------------

{- Experiment with just combinational logic, and not modeling lists
but rather having a cycle number in each node. NodeCyc is a node that
contains an explicit cycle number; this can be used to determine
whether a node has been updated to the new cycle number yet. -}

{- A SigNodeCyc represents a signal.  It uses in-place update rather
than a stream of nodes to represent the signal's value over time.  The
mutable state is a pair (cycle number, signal value).  There is no
pointer to another signal. The Component field gives the information
needed to calculate the value of this signal for a new clock cycle. -}

data SigNodeCyc = SigNodeCyc Component (IORef (Int,Int))
data Component
  = Component1 (Int->Int) SigNodeCyc
  | Component2 (Int->Int->Int) SigNodeCyc SigNodeCyc

{- need to define an inport, an inv, and2.  When we use inv x = map
invstatic x, this is attaching the static inv function to the signal.
The same thing needs to be done here.  -}

{- mapsnc would be used for signal instances based on SigNodeCyc, in
place of the stream map used for ordinary streams.  It is important
not to involve mapsnc with the IO monad, as that would require
IO-izing the whole circuit specification (?).  Using an Int to model a
signal. -}

-- inv :: SigNodeCyc -> SigNodeCyc

{- update takes new cycle number and a node; recalculates the node if
necessary and returns the resulting signal value for the new clock
cycle -}

update :: Int -> SigNodeCyc -> IO Int
update newcyc (SigNodeCyc cmp p) =
  do (cyc,x) <- readIORef p
     if newcyc == cyc
       then return x
       else do y <- update newcyc cmp
               return 0
  
{-
mapsnc :: (Int->Int) -> SigNodeCyc -> SigNodCyc
mapsnc f x =
-}

testSigNodeCyc :: IO ()
testSigNodeCyc = do
  x <- newIORef (0,100)  -- initial value
  let px = SigNodeCyc x    -- pointer to x signal
  y <- newIORef (0,100)  -- initial value
  let py = SigNodeCyc y    -- pointer to x signal


  putStrLn "done"



------------------------------------------------------------------------

{-
mapnodelist :: (Int->Int) -> IORef Node1 -> IORef Node1
mapnodelist r = do
  Node1 i p <- readIORef r
-}  

main :: IO ()
main = foo

{-
test1 :: IO ()
-- Read input, put into IORef, then extract it and print
test1 = do
-- make input node x0
  x0i <- newIORef 100
 
-}
  

foo = do
  p0 <- newIORef (Node1 100 undefined)

  p1 <- newIORef (Node1 101 undefined)
  Node1 p0i p0n <- readIORef p0
  writeIORef p0 (Node1 p0i p1)

  p2 <- newIORef (Node1 102 undefined)
  Node1 p1i p1n <- readIORef p1
  writeIORef p1 (Node1 p1i p2)

  printlist p0




{-
  xs <- tolist p0
  putStrLn "have xs now"
  traverse_list xs

traverse_list :: [Int] -> IO ()
traverse_list xs = do
  putStrLn (show (head xs))
  traverse_list (tail xs)

tolist :: IORef Node1 -> IO [Int]
tolist r = do
  Node1 i n <- readIORef r
  xs <- tolist n
  return (i : xs)
-}


  
