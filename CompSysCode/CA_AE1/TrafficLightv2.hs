module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational


-- limits for TrafficLight_version2
limitRed = [one, zero]      -- 2


trafficLight_version2 :: CBit a => a -> a -> (a,a,a,a,a,[a])
trafficLight_version2 reset walkRequest = (green, amber, red, wait, walk,walkCount)
  where

    -- states
    green = dff green_output
    amber = dff amber_output
    red= dff red_output
    wait = dff wait_output
    walk = dff walk_output

    -- Green State
    green_output = or3 reset (and2 green (inv walkRequest) ) (and2 amber inv (walkRequest))





    cnt = counter2b reset

    limitR_hit = and2 red (eq2 cnt limitR)
    
-- Helper functions

-- 2 bits counter to time the 3 cycle wait.
counter2b :: CBit a=> a ->[a]
counter reset = [x0, x1]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    (c1, y0) = halfAdd x0 one
    (c2, y1) = halfAdd x1 c1

eq2 :: CBit a => [a] -> [a] -> a
eq2 [a0,a1] [b0,b1] =
  and2 e0 e1
  where
    e0 = inv (xor2 a0 b0)
    e1 = inv (xor2 a1 b1)

    
    