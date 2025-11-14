module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO ()
main = driver $ do
  -- reset=1 for first cycle, then 0

testdata1 :: [String]
testdata1= [ "1", "0", "0", "0", "0", "0", "0", "0" ]

  r <- inputBit "reset"

  let [c2,c1,c0] = counter r

  outputWord "cnt"   [c2,c1,c0]
  outputBit  "r_dbg" r

  runSimulation
