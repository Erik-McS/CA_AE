module TrafficLightRun where
import HDL.Hydra.Core.Lib
import TrafficLight


testdata1 :: [String]
testdata1=
------------------------------
--   x       expected output
------------------------------
  [ "1"   --  (0,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (0,1,0)
  , "0"   --  (0,0,1)
  , "0"   --  (0,0,1)  
  , "0"   --  (0,0,1)
  , "0"   --  (0,0,1)
  , "0"   --  (0,1,0)  
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (0,1,0)
  , "0"   --  (0,0,1)
  , "0"   --  (0,0,1)  
  , "0"   --  (0,0,1)
  , "0"   --  (0,0,1)
  , "0"   --  (0,1,0)
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (1,0,0)
  , "0"   --  (0,1,0)
  , "0"   --  (0,0,1)  
  ]

main :: IO ()
main = driver $ do
-- Input data
  useData testdata1

-- Inputs
  x <- inputBit "x"

-- Circuit
  let (green_s,amber_s,red_s) = trafficLight1 x
-- Output ports
  outputBit "Green_s" green_s
  outputBit "Amber_s" amber_s
  outputBit "Red_s" red_s
--  outputWord "limit" limit
--  outputWord "cnt" cnt
--  outputBit "done" done  
-- Run
  runSimulation