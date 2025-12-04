-- Sigma16: ALUrun.hs
-- John T. O'Donnell, 2022
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

---------------------------------------------------------------------------
-- Simulation driver and test data for ALU
---------------------------------------------------------------------------

-- Usage: cd to the M1 directory and enter ghc -e main Circuit/ALUrun

module Main where
import HDL.Hydra.Core.Lib
import M1.ALU
import HDL.Hydra.Core.Lib (inputBit)

{-

Result function

| a b c |    r     |
|-------+-----------
| 0 0 0 |   x+y    | 
| 0 0 1 |   x-y    | 
| 0 1 0 |    -x    | 
| 0 1 1 |   x+1    | 
| 1 0 0 |   cmp    |

Condition code

| bit index | Relation        | Symbol |
|-----------+-----------------+--------|
|         0 | > Int           | g      |
|         1 | > Nat           | G      |
|         2 | =               | =      |
|         3 | < Nat           | L      |
|         4 | < Int           | <      |
|         5 | Int overflow    | v      |
|         6 | Nat overflow    | V      |
|         7 | Carry           | C      |
|         8 | Stack overflow  | S      |
|         9 | Stack underflow | s      |
-}

alu_input1 =
--   a  b  c  g     x      y   cc     Operation  Result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [ "0  0  0  0  14     15   0"    --   x+y       29
  , "0  0  0  0  125    590   0"    --   x+y      715
  , "0  0  0  1  0xffff  0x0000       0"    --   inv x      0x0000
  , "0  0  0  1  0x0000  0xffff       0"    --   inv x      oxffff
  , "0  0  1  1  0xffff  0xffff       0"    --   and x y      oxffff
  , "0  0  1  1  0xffff  0x0000       0"    --   and x y      ox0000
  , "0  0  1  1  0x0000  0xffff       0"    --   and x y      ox0000
  , "0  0  1  1  0x0000  0x0000       0"    --   and x y      ox0000
  , "0  1  0  1  0xffff  0x0000       0"    --   or x y      oxffff
  , "0  1  0  1  0x0000  0xffff       0"    --   or x y      oxffff
  , "0  1  0  1  0x0000  0x0000       0"    --   or x y      ox0000
  , "0  1  0  1  0xffff  0xffff       0"    --   or x y      oxffff
  , "0  1  1  1  0xffff  0x0000       0"    --   xor x y      oxffff
  , "0  1  1  1  0x0000  0xffff       0"    --   xor x y      oxffff
  , "0  1  1  1  0xffff  0xffff       0"    --   xor x y      ox0000
   ]


---------------------------------------------------------------------------
-- Simulation driver for ALU
---------------------------------------------------------------------------

main = driver  $ do

-- Word size
  let n =  16

-- Input data
  useData alu_input1

-- Input signals
  a <- inputBit "a"
  b <- inputBit "b"
  c <- inputBit "c"
  g <- inputBit "g"
  x <- inputWord "x" n
  y <- inputWord "y" n
  cc <- inputWord "cc" n

-- Circuit  
  let (r,ccnew) = alu n (a,b,c,g) x y cc

  format
      [string "Inputs:  ",
       string " abcg = ", bit a, bit b, bit c,bit g,
       string "\n         x = ", bits x, string " $", binhex x,
       string " (bin:", bindec 5 x, string ")",
       string " (tc: ", bitstc 6 x, string ")",
       string "\n         y = ", bits y, string " $", binhex y,
       string " (bin:", bindec 5 y, string ")",
       string " (tc: ", bitstc 6 y, string ")",
       string "\n       Outputs:  ",
       string "\n         r = ", bits r, tcdec 5 r, string "  $", binhex r,
       string "\n         ccnew = ", bits ccnew, string " $", binhex ccnew,
       string "\n"]
  runSimulation
