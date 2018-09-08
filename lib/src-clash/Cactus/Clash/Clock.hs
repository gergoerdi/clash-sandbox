module Cactus.Clash.Clock
    ( clkPeriod
    , clkRate
    ) where

import Clash.Prelude hiding (clkPeriod)
import qualified Cactus.Clash.Explicit.Clock as E

clkPeriod :: (HiddenClock domain gated, domain ~ Dom s ps, KnownNat ps) => Integer
clkPeriod = hideClock E.clkPeriod

clkRate :: (HiddenClock domain gated, domain ~ Dom s ps, KnownNat ps) => Integer
clkRate = hideClock E.clkRate
