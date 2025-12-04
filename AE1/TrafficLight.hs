module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational


-- controller 1 input : reset
-- output: (green,amber,red)
-- each state has a duration: G(3) A(1) R(4) A(1)
-- must keep track of amber previous/next
controller1 reset = (green,amber,red)
--vthree = 
  where
    -- green: either reset =1 or amber=1 or dwell<=3
    green = dff (or2 reset amber)
    amber = dff (and2 reset' green)
    red = dff (and2 reset' amber)
    reset' = inv reset
