{-# LANGUAGE RecordWildCards, TupleSections #-}
module Brainfuck where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import Control.Monad

import Brainfuck.Computer

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "Brainfuck"
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
          , PortName "LED"
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
      , Signal System (Vec 2 Bit)
      )
topEntity = exposeClockReset board
  where
    board recv switches btn0 = (txOut <$> serialOut, (anodes, segments, dp), leds)
      where
        dim s = (.&&.) <$> (repeat <$> countTo 64) <*> s
        anodes = activeLow <$> dim (ssMask <$> ss)
        segments = ssSegments <$> ss'
        dp = ssDP <$> ss'

        leds = bundle $ activeHigh (isJust <$> output)  :> activeHigh needInput :> Nil

        btn = fmap (fromMaybe False) . debounce d16 . fmap (bitToBool . complement) $ btn0
        click = isRising maxBound btn

        rawInput = unpack . v2bv <$> switches
        ackOutput = click .&&. fifoReady
        input = mplus <$> (enable <$> click <*> rawInput) <*> serialIn

        serialIn = rx clkRate serialRate recv

        serialOut = tx clkRate serialRate output'
        (output', fifoReady) = fifo (diff output) (txReady <$> serialOut)

        (output, needInput, cpuState) = computer "prog.rom" input ackOutput

        (ihi, ilo) = unbundle $ splitByte <$> rawInput
        (ohi, olo) = unbundle $ splitByte . fromMaybe 0 <$> output
        -- (ohi, olo) = unbundle $ splitByte . showPC <$> cpuPC <$> cpuState
        -- output = showPC <$> cpuOutPC <$> cpuOut
        -- (ihi, ilo) = unbundle $ splitByte . showPC <$> (cpuOutPC <$> cpuOut)

        digits = ilo :> ihi :> olo :> ohi :> Nil
        ss = driveSS 10000 (bundle digits)

        ss' = activeLow <$> ss

        noSegs = pure (repeat low)

clkRate :: Word32
clkRate = 32000000

serialRate :: Word32
serialRate = 9600
