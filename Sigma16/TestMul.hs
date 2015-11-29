------------------------------------------------------------------------
--  ArrayMax: machine language program for the Sigma16 architecture
------------------------------------------------------------------------

{- A machine language program for the Sigma16 architecture that
searches an array of natural numbers for the maximal element.  The
loop terminates when a negative element is encountered. -}

module Main where
import M1run

main :: IO ()
main = run_Sigma16_program testmul 200

------------------------------------------------------------------------

testmul :: [String]
testmul =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                  -- 0000 ; Initialise
                  -- 0000
  "f100", "0002",  -- 0000 start lea R1,2[R0]       ; R1 = constant 2
  "f200", "0004",  -- 0002       lea R2,2[R0]       ; R2 = constant 2
  "2321",         -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "0006",  -- 0005       lea R4,4[R0]       ; R4 = constant 4

                  -- 0007 loop if (r3!=r4)
  "5534",         -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 4
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"         -- 000a       trap  R0,R0,R0     ; else terminate
   ]

------------------------------------------------------------------------
