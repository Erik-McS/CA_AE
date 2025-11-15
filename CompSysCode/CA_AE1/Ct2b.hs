module Ct2b where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational


counter2b :: CBit a=> a ->[a]
counter2b reset = [x0, x1]
  where
    x0 = dff (mux1 reset y0 zero)
    x1 = dff (mux1 reset y1 zero)
    (c1, y0) = halfAdd x0 c2
    (c2, y1) = halfAdd x1 one