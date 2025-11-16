module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational



-- duration limits - they are used to produce a pattern
-- here -> GGGARRRRA
limitG, limitA, limitR :: CBit a => [a]
limitG = [zero, one, zero]     -- 2
limitA = [zero, zero, zero]    -- 0
limitR = [one, one, zero]      -- 3

trafficLight_version1 :: CBit a => a -> (a,a,a)
trafficLight_version1 reset = (green, amber, red)
  where

    -- states, using dffs
    green = dff green_next
    amber = dff amber_next
    red= dff red_next

    -- Transitions --

    -- Green_next: Reset takes precedence, otherwise stay green or transition to amber
    green_next = or3 reset (and2 green (inv done)) (and3 amber done dir)

    -- amber_next: If not resetting, use transition logic
    -- we go amber if green sequence or from red, or from amber if not done (in case the amber sequence is changed)
    amber_next_logic = or3 (and2 green done) (and2 red done) (and2 amber (inv done))
    amber_next = and2 (inv reset) amber_next_logic
    
    -- red_next: If not resetting, use transition logic
    -- we go red if from aamber and dir flag 
    red_next_logic = or2 (and3 amber done (inv dir)) (and2 red (inv done))
    red_next = and2 (inv reset) red_next_logic

    -- counter - used to repeat states
    (cnt, nextCnt) = counter clear

    -- we need to decide which state between G and R to go after amber -> need memory(dff)
    -- compute a flag for the next colour after amber (dirNext)
    -- If doneRed=1, dirNext=1. Else if doneGreen=1, dirNext=0. Else, hold 'dir'.
    isDoneRedActive = and2 red done 
    dirNext = mux1 isDoneRedActive (mux1 doneGreen dir zero) one
    -- reset the colour flag if reset=1 (forces dir=0 if reset=1)
    dirNext_resettable = mux1 reset dirNext zero
    -- The DFF output is based on the resettable input
    dir = dff dirNext_resettable
    
--  compute limits
    limitG_hit = and2 green (eq3 cnt limitG)
    limitA_hit = and2 amber (eq3 cnt limitA)
    limitR_hit = and2 red   (eq3 cnt limitR)
    
    -- done = we reached the limit for the active state
    done = or3 limitG_hit limitA_hit limitR_hit
    
    -- intermediate states
    doneGreen = limitG_hit
    doneAmber = limitA_hit
    doneRed   = limitR_hit

    -- state change - we decide here if we change states or reset.
    -- as the counter is set to 000 at each state change
    change = or3 doneGreen doneAmber doneRed
    clear = or2 change reset

-- Helper functions
-- counter block
counter :: CBit a => a -> ([a],[a])
counter reset = ([x0, x1, x2], [y0, y1, y2])
  where
    (c1, y0) = halfAdd x0 one
    (c2, y1) = halfAdd x1 c1
    (_,  y2) = halfAdd x2 c2

    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)

-- didnt found an equality function. so that added to the circuit here.
-- eq 3 bits
eq3 :: CBit a => [a] -> [a] -> a
eq3 [a0,a1,a2] [b0,b1,b2] =
  and2 (and2 e0 e1) e2
  where
    e0 = inv (xor2 a0 b0)
    e1 = inv (xor2 a1 b1)
    e2 = inv (xor2 a2 b2)

-- TrafficLight_version2

trafficLight_version2 :: CBit a => a -> a -> (a,a,a,a,a,[a])
trafficLight_version2 reset walkRequest = (green, amber, red, wait, walk,walkCount)
  where

  -- to delete, for FSM test only  
  -- walkCount = [zero,zero,zero]  
  -- light outputs
  green = sGreen
  amber = or2 sAmberBefore sAmberAfter
  red   = or3 sRed1 sRed2 sRed3


  -- states
  sGreen       = dff sGreen_next
  sAmberBefore = dff sAmberBefore_next
  sRed1        = dff sRed1_next
  sRed2        = dff sRed2_next
  sRed3        = dff sRed3_next
  sAmberAfter  = dff sAmberAfter_next


  -- transitions

  sGreen_next = or3 reset (and2 sGreen (inv walkRequest)) sAmberAfter
  sAmberBefore_next = and3 (inv reset) sGreen walkRequest
  sRed1_next = and2 (inv reset) sAmberBefore 
  sRed2_next = and2 (inv reset) sRed1 
  sRed3_next = and2 (inv reset) sRed2 
  sAmberAfter_next = and2 (inv reset) sRed3 

  walk = or3 sRed1 sRed2 sRed3
  wait = inv walk

  -- walkRequest counter
  walkCount = walkCount_reg16
  -- define 0 as with 16 bits
  zero16 = replicate 16 zero
  -- get the 16bits register file
  walkCount_reg16  = mapn dff 16 walkCount_next
  -- counter
  (c,inc_counter) = rippleAdd one (zip walkCount_reg16 zero16)
  -- if walkRequest -> ++ else same value
  incOrSame = mux1w walkRequest walkCount_reg16 inc_counter
  -- if reset = 1, set the counter to 0 else counter
  walkCount_next = mux1w reset incOrSame zero16



    