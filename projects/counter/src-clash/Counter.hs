{-# LANGUAGE RecordWildCards, TupleSections #-}
module Counter where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment

import Data.Word
import Data.Maybe (fromMaybe, isJust)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "Counter"
    , t_inputs =
          [ PortName "CLK_32MHZ"
          , PortName "RESET"
          , PortName "BUTTON"
          ]
    , t_output = PortProduct ""
          [ PortProduct "" [PortName "SS_ANODES", PortName "SS_SEGS", PortName "SS_DP"]
          , PortName "LED"
          ]
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System Bit
    -> ( (Signal System (Vec 4 Bit), Signal System (Vec 7 Bit), Signal System Bit)
      , Signal System (Vec 1 Bit)
      )
topEntity = exposeClockReset board

board
    :: forall domain gated synchronous. (HiddenClockReset domain gated synchronous)
    => Signal domain Bit
    -> ( (Signal domain (Vec 4 Bit), Signal domain (Vec 7 Bit), Signal domain Bit)
      , Signal domain (Vec 1 Bit)
      )
board btn0 = ((anodes, segments, dp), singleton <$> led)
  where
    dim s = (.&&.) <$> (repeat <$> countTo 64) <*> s
    anodes = activeLow <$> dim (ssMask <$> ss)
    segments = ssSegments <$> ss'
    dp = ssDP <$> ss'
    led = activeHigh btn

    btn = debounce d16 False $ bitToBool . complement <$> btn0

    cnt = countWhen . isRising maxBound $ btn
    digits = reverse . unpack . pack <$> cnt
    ss = driveSS 10000 digits
    ss' = activeLow <$> ss

clkRate :: Word32
clkRate = 32000000
