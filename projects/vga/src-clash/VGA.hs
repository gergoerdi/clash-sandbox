{-# LANGUAGE RecordWildCards, TupleSections #-}
module VGA where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Cactus.Clash.VGA
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (guard)
import Data.Function

-- 25.175 MHz clock, needed for the VGA mode we use
type Dom25 = Dom "CLK_25MHZ" 39721

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
    :: Clock Dom25 Source
    -> Reset Dom25 Asynchronous
    -> Signal Dom25 Bit
    -> ( Signal Dom25 Bit
      , Signal Dom25 Bit
      , (Signal Dom25 Bit, Signal Dom25 Bit, Signal Dom25 (Unsigned 3), Signal Dom25 (Unsigned 3), Signal Dom25 (Unsigned 2))
      )
topEntity = exposeClockReset board
  where
    board rxIn = (pure low, activeLow led', (vgaVSync, vgaHSync, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60
        led' = regEn False (countTo $ fromIntegral clkRate) $ not <$> led'

        vgaX' = (chipX =<<) <$> vgaX
        vgaY' = (chipY =<<) <$> vgaY
        visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

        pixel = pixelAt <$> vgaX' <*> vgaY'
          where
            pixelAt (Just x) (Just y) = case (x, y) of
                (0, 0) -> True
                (1, 0) -> True
                (2, 1) -> True

                (61, 30) -> True
                (62, 31) -> True
                (63, 31) -> True
                _ -> False
            pixelAt _ _ = False

        vgaR = monochrome <$> pixel
        vgaG = monochrome <$> pixel
        vgaB = monochrome <$> pixel

monochrome :: (Bounded a) => Bool -> a
monochrome b = if b then maxBound else minBound

chipX :: Unsigned 10 -> Maybe (Unsigned 6)
chipX x = let (x', _) = unpack . pack $ x :: (Unsigned 7, Unsigned 3)
          in enable (8 <= x' && x' < 8 + 64) (truncateB $ x' - 8)

chipY :: Unsigned 10 -> Maybe (Unsigned 5)
chipY y = let (y', _) = unpack . pack $ y :: (Unsigned 7, Unsigned 3)
          in enable (14 <= y' && y' < 14 + 32) (truncateB $ y' - 14)

serialRate :: Word32
serialRate = 9600
