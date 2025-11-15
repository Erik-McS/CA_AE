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

testdata2 :: [String]
testdata2=
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
  , "1"   --  (0,0,1)
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
main = do 

  putStrLn "--- TrafficLight version 1"  
  driver $ do
  -- Input data
    useData testdata1
  -- Inputs
    x <- inputBit "x"
  -- Circuit
    let (green,amber,red) = trafficLight_version1 x
  -- Output ports
    outputBit "Green:" green
    outputBit "Amber:" amber
    outputBit "Red:" red

-- Run
    runSimulation

  putStrLn "--- TrafficLight version 2"  
  driver $ do
  -- Input data
    useData testdata2
  -- Inputs
    x <- inputBit "x"
  -- Circuit
    let (green_s,amber_s,red_s) = trafficLight_version1 x
  -- Output ports
    outputBit "Green:" green_s
    outputBit "Amber:" amber_s
    outputBit "Red:" red_s
-- Run
    runSimulation