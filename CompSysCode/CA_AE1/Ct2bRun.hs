module Ct2bRun where
import HDL.Hydra.Core.Lib
import Ct2b

testdata1 :: [String]
testdata1 = [ "1","0","0","0","0","0","0","0","0","0" ]

main :: IO ()
main = driver $ do
  useData testdata1

  rst <- inputBit "reset"

  -- NOTE: match in LSB → MSB order
  let [c0,c1] = counter2b rst

  -- And print in the same order
  outputWord "cnt" [c0,c1]


  runSimulation