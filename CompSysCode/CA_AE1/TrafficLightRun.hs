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

testdata3 :: [String]
testdata3 =
  --------------------------------
  --  rst  rWalk   expected (G,A,R)
  --------------------------------
  [ "1 0"   -- (0,0,0)  reset cycle
  , "0 0"   -- (1,0,0)  Green
  , "0 0"   -- (1,0,0)  Green
  , "0 0"   -- (1,0,0)  Green
  , "0 1"   -- (1,0,0)  Green (walkRequest sampled)
  , "0 0"   -- (0,1,0)  AmberBefore
  , "0 0"   -- (0,0,1)  Red1
  , "0 0"   -- (0,0,1)  Red2
  , "0 0"   -- (0,0,1)  Red3
  , "0 0"   -- (0,1,0)  AmberAfter
  , "0 0"   -- (1,0,0)  Green
  , "0 1"   -- (1,0,0)  Green (walkRequest again)
  , "0 0"   -- (0,1,0)  AmberBefore
  , "0 0"   -- (0,0,1)  Red1
  , "0 1"   -- (0,0,1)  Red2 (walkrequest outside Green, walkcounter++, so impact on FSM.)
  , "0 0"   -- (0,0,1)  Red3
  , "0 1"   -- (1,0,0)  Green
  , "0 0"   -- (0,1,0)  AmberBefore  (walkRequest was 1 last cycle)
  , "0 0"   -- (0,0,1)  Red1
  , "0 0"   -- (0,0,1)  Red2
  , "0 0"   -- (0,0,1)  Red3
  , "0 0"   -- (0,1,0)  AmberAfter
  , "0 0"   -- (1,0,0)  Green
  , "0 0"   -- (1,0,0)  Green
  , "0 0"   -- (1,0,0)  Green
  ]

main :: IO ()
main = do
  -- Part one 
  putStrLn "--- TrafficLight version 1"
  driver $ do
      useData testdata1
      x   <- inputBit "x"
      let (green, amber, red) =
            trafficLight_version1 x
      format
        [ string "In("
        , bit x
        , string ")  Out("
        , bit green, string ","
        , bit amber, string ","
        , bit red
        , string ")\n"
          ]

      runSimulation
  
  -- part 2
  putStrLn "--- TrafficLight version 2"
  driver $ do
      useData testdata3
      rst   <- inputBit "rst"
      rWalk <- inputBit "rWalk"
      let (green, amber, red, wait, walk, walkCount) =
            trafficLight_version2 rst rWalk
      format
        [ string "In("
        , bit rst, string ",", bit rWalk, string ")  "
        , string "Out("
        , bit green, string ",", bit amber, string ",", bit red, string ")  "
        , string "Wait=", bit wait
        , string " Walk=", bit walk
        , string " Count=", bindec 16 walkCount
        , string "\n"
        ]

      runSimulation
