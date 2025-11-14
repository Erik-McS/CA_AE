module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational



-- duration limits - they are used to produce a pattern
-- here -> GGGARRRRA
limitG, limitA, limitR :: CBit a => [a]
limitG = [zero, one, zero]     -- 2
limitA = [zero, zero, zero]    -- 0
limitR = [one, one, zero]      -- 3

trafficLight1 :: CBit a => a -> (a,a,a)
trafficLight1 reset = (green_s, amber_s, red_s)
  where

    -- states
    green_s = dff green_next
    amber_s = dff amber_next
    red_s= dff red_next

    -- Green_next: Reset takes precedence, otherwise stay or transition in
    green_next = or3 reset (and2 green_s (inv done)) (and3 amber_s done dir)

    -- amber_next: If not resetting, use transition logic
    amber_next_logic = or3 (and2 green_s done) (and2 red_s done) (and2 amber_s (inv done))
    amber_next = and2 (inv reset) amber_next_logic
    
    -- red_next: If not resetting, use transition logic
    red_next_logic = or2 (and3 amber_s done (inv dir)) (and2 red_s (inv done))
    red_next = and2 (inv reset) red_next_logic

    -- counter - used to repeat states
    (cnt, nextCnt) = counter clear

    -- we need to decide which state between G and R to go after amber -> need memory(dff)
    
    -- 1. Calculate the next desired value (dirNext_comb)
    -- If doneRed=1, dirNext_comb=1. Else if doneGreen=1, dirNext_comb=0. Else, hold 'dir'.
    isDoneRedActive = and2 red_s done 
    dirNext_comb = mux1 isDoneRedActive (mux1 doneGreen dir zero) one
    
    -- 2. Apply the synchronous reset logic (forces dir=0 if reset=1)
    dirNext_resettable = mux1 reset dirNext_comb zero
    
    -- 3. The DFF output is based on the resettable input
    dir = dff dirNext_resettable
    
-- limit is no longer an output/variable, it is just used for clarity
    limitG_hit = and2 green_s (eq3 cnt limitG)
    limitA_hit = and2 amber_s (eq3 cnt limitA)
    limitR_hit = and2 red_s   (eq3 cnt limitR)
    
    -- done = we reached the limit for the active state
    done = or3 limitG_hit limitA_hit limitR_hit
    
    -- intermediate states (change these to use the new signals)
    doneGreen = limitG_hit
    doneAmber = limitA_hit
    doneRed   = limitR_hit

    -- state change - we decide here if we change states or reset.
    -- as the counter is set to 000 at each state change
    change = or3 doneGreen doneAmber doneRed
    clear = or2 change reset


{-This is the counter block. used to simulate duration
start at 0, incremented by one at each cycle. is reset when the state changes, using the clear signal.-}
-- +1 block
plusOne :: CBit a => [a] -> [a]
plusOne [x2,x1,x0] = [p2,p1,p0]
  where
    c0 = x0
    p0 = inv x0

    c1 = and2 c0 x1
    p1 = xor2 x1 c0

    c2 = and2 c1 x2
    p2 = xor2 x2 c1
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

    
    