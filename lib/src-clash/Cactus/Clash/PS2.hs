{-# LANGUAGE RecordWildCards #-}
module Cactus.Clash.PS2 (samplePS2) where

import Clash.Prelude
import Cactus.Clash.Util
import Data.Word

data PS2 dom = PS2
    { ps2Clk :: Signal dom Bit
    , ps2Data :: Signal dom Bit
    }

samplePS2
    :: (HiddenClockReset dom gated synchronous)
    => PS2 dom -> Signal dom (Maybe Bit)
samplePS2 PS2{..} = enable <$> isFalling low ps2Clk' <*> ps2Data'
  where
    stableClk = fst $ regShiftIn (replicate d8 low) (Just <$> ps2Clk)
    stablePattern = fst $ regShiftIn (replicate d8 low) (Just <$> ps2Data)

    ps2Clk' = regMaybe low $ extremum <$> stableClk
    ps2Data' = regMaybe low $ extremum <$> stableClk
