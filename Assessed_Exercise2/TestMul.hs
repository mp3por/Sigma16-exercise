------------------------------------------------------------------------
-- TestMul - a test program used to test if the Multiply functional unit
-- has been correctly included into the M1 system.

-- The Multiply function unit is added to the system in order for the
-- circuit to handle big calculations like multiplication without the
-- need of slowing down the clock speed. In the ideal case the circuit
-- will schedule a multiplication on the functional unit and continue
-- with its work while the functional unit does the calculation. This
-- will allow the clock speed to remain closer to the amount of time
-- needed by the ALU to make simple calculations like add,compare,remove
-- rather than having to slow down to wait for a multiplication and thus
-- keep the system fast and responsive. However a full asynchronous
-- implementation for a functional unit will require a way to handle
-- data dependencies between instructions which hard and not in the
-- scope of this assignment. Therefore the current M1 circuit takes
-- full advantage of the functional unit, by leaving the multiplication
-- to it, and keeps its clock speed the same, but stalls until the unit
-- is ready with the result.

-- A multiplication takes a variable amount of cycles because it
-- involves a pipeline of rippleAdders. The general idea behind
-- multiplication 'x*y' is summing 'x' with itself 'y' times and if we
-- consider an addition takes one clock cycle we can see why a
-- multiplication takes a lot more.
------------------------------------------------------------------------

module TestMul where
import HDL.Hydra.Core.Lib
import M1run

-- Print a line to separate output from different simulations
separator :: [Char] -> IO()
separator prog_name =
  do
    putStrLn (take 90 (repeat '-'))
    putStrLn ("Starting new test")
    putStrLn ("Test name: " ++ prog_name)
    putStrLn (take 90 (repeat '-'))

main :: IO ()
main =
  do
    separator "test_small_positive_numbers"
    run_Sigma16_program test_small_positive_numbers 200

    separator "test_small_negative_numbers"
    run_Sigma16_program test_small_negative_numbers 200

    separator "test_big_positive_numbers"
    run_Sigma16_program test_big_positive_numbers 200

    separator "test_positive_negative_numbers"
    run_Sigma16_program test_positive_negative_numbers 200

    separator "test_positive_negative_numbers2"
    run_Sigma16_program test_positive_negative_numbers2 200

    separator "test_big_positive_numbers_overflow"
    run_Sigma16_program test_big_positive_numbers_overflow 200

    separator "test_big_negative_numbers_overflow"
    run_Sigma16_program test_big_negative_numbers_overflow 200



------------------------------------------------------------------------

-- test for small positive numbers
test_small_positive_numbers :: [String]
test_small_positive_numbers =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "0002",  -- 0000 start lea R1,2[R0]       ; R1 = constant 2
  "f200", "0004",  -- 0002       lea R2,2[R0]       ; R2 = constant 2
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "0008",  -- 0005       lea R4,4[R0]       ; R4 = constant 4

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 4
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]

------------------------------------------------------------------------

-- test for negative numbers
test_small_negative_numbers :: [String]
test_small_negative_numbers =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "fffe",  -- 0000 start lea R1,2[R0]       ; R1 = constant -2
  "f200", "fffe",  -- 0002       lea R2,2[R0]       ; R2 = constant -2
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "0004",  -- 0005       lea R4,4[R0]       ; R4 = constant 4

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 4
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]


------------------------------------------------------------------------

-- test for big positive numbers
test_big_positive_numbers :: [String]
test_big_positive_numbers =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "0096",  -- 0000 start lea R1,2[R0]       ; R1 = constant 150
  "f200", "0064",  -- 0002       lea R2,2[R0]       ; R2 = constant 100
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "3a98",  -- 0005       lea R4,4[R0]       ; R4 = constant 15000

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 15000
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]


------------------------------------------------------------------------

-- test for negative * positive numbers
test_positive_negative_numbers :: [String]
test_positive_negative_numbers =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "fffe",  -- 0000 start lea R1,2[R0]       ; R1 = constant -2
  "f200", "0002",  -- 0002       lea R2,2[R0]       ; R2 = constant  2
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "fffc",  -- 0005       lea R4,4[R0]       ; R4 = constant -4

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == -4
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]


------------------------------------------------------------------------

-- test for negative * positive numbers2
test_positive_negative_numbers2 :: [String]
test_positive_negative_numbers2 =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "fffe",  -- 0000 start lea R1,2[R0]       ; R1 = constant -2
  "f200", "0006",  -- 0002       lea R2,2[R0]       ; R2 = constant  6
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "fff4",  -- 0005       lea R4,4[R0]       ; R4 = constant -12

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == -12
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]



------------------------------------------------------------------------

-- test for big positive numbers
-- NOTE:
-- this test shows how my implementation overflows. This happens because the
-- multiplier takes 2 16bits long words and produces a product which is 32bits
-- wide. However our registry holds only 16bit words so on when saving the result
-- we only take the 16 least significant bits from the product and thus the overflow


test_big_positive_numbers_overflow :: [String]
test_big_positive_numbers_overflow =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "8000",  -- 0000 start lea R1,2[R0]       ; R1 = constant 32768
  "f200", "0002",  -- 0002       lea R2,2[R0]       ; R2 = constant 2
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "0000",  -- 0005       lea R4,4[R0]       ; R4 = constant 0 (and the real result should be f0000)

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 15000
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]


------------------------------------------------------------------------

-- test for big negative numbers
-- NOTE:
-- this test shows how my implementation overflows. This happens because the
-- multiplier takes 2 16bits long words and produces a product which is 32bits
-- wide. However our registry holds only 16bit words so on when saving the result
-- we only take the 16 least significant bits from the product and thus the overflow


test_big_negative_numbers_overflow :: [String]
test_big_negative_numbers_overflow =
-- Machine Language  Addr    Assembly Language     Comment
-- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 [
                   -- 0000 ; Initialise
                   -- 0000
  "f100", "8001",  -- 0000 start lea R1,2[R0]       ; R1 = constant -32767
  "f200", "0002",  -- 0002       lea R2,2[R0]       ; R2 = constant 2
  "2321",          -- 0004       mul R3,R2,R1       ; R3 = R2*R1
  "f400", "0000",  -- 0005       lea R4,4[R0]       ; R4 = constant 0 (should be -65534)

                   -- 0007 loop if (r3!=r4)
  "5534",          -- 0007       cmpeq R5,R3,R4     ; R5 = R3 == 0
  "f504", "0007",  -- 0008 then  jumpf R5,loop[R0]  ; if r3!=r4 goto loop
  "d000"           -- 000a       trap  R0,R0,R0     ; else terminate
   ]

