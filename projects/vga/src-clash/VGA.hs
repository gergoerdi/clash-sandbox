{-# LANGUAGE RecordWildCards, TupleSections #-}
module VGA where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (guard)
import Data.Function

import VGADriver

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "VGA"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "RX"
          ]
    , t_output = PortProduct ""
          [ PortName "TX"
          , PortName "LED"
          , PortProduct "" [ PortName "VGA_VSYNC", PortName "VGA_HSYNC", PortName "VGA_RED", PortName "VGA_GREEN", PortName "VGA_BLUE" ]
          ]
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System Bit
    -> ( Signal System Bit
      , Signal System Bit
      , (Signal System Bit, Signal System Bit, Signal System (Unsigned 3), Signal System (Unsigned 3), Signal System (Unsigned 2))
      )
topEntity = exposeClockReset board
  where
    board rxIn = (pure low, activeLow led', (vgaVSync, vgaHSync, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60
        -- led' = regEn False (vgaStartFrame .&&. ledcnt .==. 0) (not <$> led')
        -- ledcnt = regEn (0 :: Word8) vgaStartFrame $ mux (ledcnt .==. 59) 0 (ledcnt + 1)
        led' = regEn False (countTo clkRate) $ not <$> led'

        vgaX' = (chipX =<<) <$> vgaX
        vgaY' = (chipY =<<) <$> vgaY

        vgaR = maybe 0 truncateB <$> vgaX'
        vgaG = maybe 0 truncateB <$> vgaY'
        vgaB = pure 0

chipX :: Unsigned 10 -> Maybe (Unsigned 6)
chipX x = let (x', _) = unpack . pack $ x :: (Unsigned 7, Unsigned 3)
          in enable (8 <= x' && x' < 8 + 64) (truncateB $ x' - 8)

chipY :: Unsigned 10 -> Maybe (Unsigned 5)
chipY y = let (y', _) = unpack . pack $ y :: (Unsigned 7, Unsigned 3)
          in enable (14 <= y' && y' < 14 + 32) (truncateB $ y' - 14)

clkRate :: Word32
clkRate = 25175000

serialRate :: Word32
serialRate = 9600
