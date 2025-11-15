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

    -- Green_next: Reset takes precedence, otherwise stay or transition in
    green_next = or3 reset (and2 green (inv done)) (and3 amber done dir)

    -- amber_next: If not resetting, use transition logic
    amber_next_logic = or3 (and2 green done) (and2 red done) (and2 amber (inv done))
    amber_next = and2 (inv reset) amber_next_logic
    
    -- red_next: If not resetting, use transition logic
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
    
    -- 3. The DFF output is based on the resettable input
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

    
    