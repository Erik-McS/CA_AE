module SpRun where
import HDL.Hydra.Core.Lib
import SimpleCounter

testdata1 :: [String]
testdata1 = [ "1","0","0","0","0","0","0","0","0","0" ]

main :: IO ()
main = driver $ do
  useData testdata1

  rst <- inputBit "reset"

  -- NOTE: match in LSB → MSB order
  let [c0,c1,c2] = counter3 rst

  -- And print in the same order
  outputWord "cnt" [c2,c1,c0]


  runSimulation
