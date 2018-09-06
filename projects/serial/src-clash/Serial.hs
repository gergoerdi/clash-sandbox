{-# LANGUAGE RecordWildCards, TupleSections #-}
module Serial where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Data.Word
import Data.Maybe (fromMaybe, isJust)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "Serial"
    , t_inputs =
          [ PortName "CLK_32MHZ"
          , PortName "RESET"
          , PortName "RX"
          , PortName "SWITCHES"
          , PortName "BUTTON"
          ]
    , t_output = PortProduct ""
          [ PortName "TX"
          , PortProduct "" [PortName "SS_ANODES", PortName "SS_SEGS", PortName "SS_DP"]
          ]
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System Bit
    -> Signal System (Vec 8 Bit)
    -> Signal System Bit
    -> ( Signal System Bit
      , (Signal System (Vec 4 Bit), Signal System (Vec 7 Bit), Signal System Bit)
      )
topEntity = exposeClockReset board
  where
    board rxIn switches btn0 = (txOut <$> serialTX, (anodes, segments, dp))
      where
        dim s = (.&&.) <$> (repeat <$> countTo 64) <*> s
        anodes = activeLow <$> dim (ssMask <$> ss)
        segments = ssSegments <$> ss'
        dp = ssDP <$> ss'

        btn = debounce d16 False . fmap (bitToBool . complement) $ btn0
        click = isRising maxBound btn

        rawOutput = unpack . v2bv <$> switches
        output = gate <$> click <*> rawOutput

        serialTX = tx clkRate serialRate output'
        (output', fifoReady) = fifo (diff output) (txReady <$> serialTX)

        input = regMaybe 0 $ rx clkRate serialRate rxIn
        -- input = pure 0

        (ihi, ilo) = unbundle $ splitByte <$> input
        (ohi, olo) = unbundle $ splitByte <$> rawOutput
        -- (ohi, olo) = unbundle $ splitByte . showPC <$> cpuPC <$> cpuState
        -- output = showPC <$> cpuOutPC <$> cpuOut
        -- (ihi, ilo) = unbundle $ splitByte . showPC <$> (cpuOutPC <$> cpuOut)

        digits = ilo :> ihi :> olo :> ohi :> Nil
        ss = driveSS 10000 (bundle digits)

        ss' = activeLow <$> ss

        noSegs = pure (repeat low)

gate :: Bool -> a -> Maybe a
gate False = const Nothing
gate True = Just

clkRate :: Word32
clkRate = 32000000

serialRate :: Word32
serialRate = 9600
