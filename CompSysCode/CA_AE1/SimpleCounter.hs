module SimpleCounter where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

counter3 :: CBit a => a -> [a]
counter3 reset = [x0, x1, x2]
  where
    (c1, y0) = halfAdd x0 one
    (c2, y1) = halfAdd x1 c1
    (_,  y2) = halfAdd x2 c2

    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    x2 = dff (mux1 reset y2 zero)

