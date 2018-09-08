module Cactus.Clash.Explicit.Clock
    ( clkPeriod
    , clkRate
    ) where

import Clash.Prelude hiding (clkPeriod)
import Clash.Signal.Internal (Clock(..))

clkPeriod :: forall s ps gated. (KnownNat ps) => Clock (Dom s ps) gated -> Integer
clkPeriod clk = snatToInteger (SNat @ps)

-- -- https://github.com/clash-lang/clash-compiler/issues/348
-- clkPeriod :: Clock dom gated -> Integer
-- clkPeriod (Clock _ period) = snatToInteger period
-- clkPeriod (GatedClock _ period _) = snatToInteger period

clkRate :: (KnownNat ps) => Clock (Dom s ps) gated -> Integer
clkRate clk = 10^12 `div` clkPeriod clk
