{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Serial where

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy

type Dom32 = Dom "CLK_32MHZ" 31250

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
    :: Clock Dom32 Source
    -> Reset _ Asynchronous
    -> Signal _ Bit
    -> Signal _ (Vec 8 Bit)
    -> Signal _ Bit
    -> ( Signal _ Bit
      , (Signal _ (Vec 4 Bit), Signal _ (Vec 7 Bit), Signal _ Bit)
      )
topEntity = exposeClockReset board
  where
    board rxIn switches btn0 = (txOut, (anodes, segments, dp))
      where
        dim s = (.&&.) <$> (repeat <$> countTo 64) <*> s
        anodes = activeLow <$> dim (ssMask <$> ss)
        segments = ssSegments <$> ss'
        dp = ssDP <$> ss'

        btn = debounce d16 False . fmap (bitToBool . complement) $ btn0
        click = isRising maxBound btn

        rawOutput = unpack . v2bv <$> switches
        output = gate <$> click <*> rawOutput

        TXOut{..} = tx serialRate output'
        (output', fifoReady) = fifo (diff output) txReady

        input = regMaybe 0 $ rx serialRate rxIn
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

serialRate :: Word32
serialRate = 9600
