{-# LANGUAGE RecordWildCards, TupleSections #-}
module Brainfuck where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.SevenSegment
import Cactus.Clash.SerialTX
import Data.Word
import Data.Maybe (fromMaybe, isJust)

import Brainfuck.Computer

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "Brainfuck"
    , t_inputs =
          [ PortName "CLK_32MHZ"
          , PortName "RESET"
          , PortName "SWITCHES"
          , PortName "BUTTON"
          ]
    , t_output = PortProduct ""
          [ PortProduct "" [PortName "SS_ANODES", PortName "SS_SEGS", PortName "SS_DP"]
          , PortName "LED"
          , PortName "TX"
          ]
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System (Vec 8 Bit)
    -> Signal System Bit
    -> ( (Signal System (Vec 4 Bit), Signal System (Vec 7 Bit), Signal System Bit)
      , Signal System (Vec 1 Bit)
      , Signal System Bit
      )
topEntity = exposeClockReset board

board
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Vec 8 Bit)
    -> Signal domain Bit
    -> ( (Signal domain (Vec 4 Bit), Signal domain (Vec 7 Bit), Signal domain Bit)
      , Signal domain (Vec 1 Bit)
      , Signal domain Bit
      )
board switches btn0 = ((anodes, segments, dp), singleton <$> led, serialOut)
  where
    dim s = (.&&.) <$> (repeat <$> countTo 64) <*> s
    anodes = activeLow <$> dim (ssMask <$> ss)
    segments = ssSegments <$> ss'
    dp = ssDP <$> ss'
    serialOut = txOut <$> tx clkRate serialRate output
    led = activeHigh $ isJust <$> output

    btn = fmap (fromMaybe False) . debounce d16 . fmap (bitToBool . complement) $ btn0

    rawInput = unpack . v2bv <$> switches
    input = gate <$> btn <*> rawInput
    ackOutput = rising btn

    (output, needInput) = computer "prog.rom" input ackOutput

    (ihi, ilo) = unbundle $ splitByte <$> rawInput
    (ohi, olo) = unbundle $ splitByte . fromMaybe 0 <$> output
    -- (ohi, olo) = unbundle $ splitByte . unpack . pack <$> (cpuOutPC <$> cpuOut)

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
