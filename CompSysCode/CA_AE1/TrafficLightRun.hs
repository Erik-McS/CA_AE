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
  --outputWord "cnt" (reverse cnt)
  --outputBit  "clr" clear
  --outputBit  "done" done
  -- outputBit  "dir" dir
  --outputBit "doneGreen" doneGreen
  --outputBit "doneRed" doneRed
  --outputBit "doneAmber" doneAmber

-- Run
  runSimulation