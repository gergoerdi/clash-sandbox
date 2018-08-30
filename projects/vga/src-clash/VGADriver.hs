{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VGADriver where

import Clash.Prelude
import Cactus.Clash.Util
import Data.Word

data VGATiming n = VGATiming
    { visibleSize, pre, syncPulse, post :: Unsigned n
    }
    deriving (Show)

data VGATimings w h = VGATimings
    { vgaHorizTiming :: VGATiming w
    , vgaVertTiming :: VGATiming h
    }
    deriving (Show)

data VGADriver dom w h = VGADriver
    { vgaVSync :: Signal dom Bit
    , vgaHSync :: Signal dom Bit
    , vgaStartFrame :: Signal dom Bool
    , vgaStartLine :: Signal dom Bool
    , vgaX :: Signal dom (Maybe (Unsigned w))
    , vgaY :: Signal dom (Maybe (Unsigned h))
    }

vgaDriver :: forall dom gated synchronous w h.
    (HiddenClockReset dom gated synchronous, KnownNat w, KnownNat h)
    => VGATimings w h
    -> VGADriver dom w h
vgaDriver VGATimings{..} = VGADriver{..}
  where
    vgaVSync = activeLow $ pure vSyncStart .<=. vCount .&&. vCount .<. pure vSyncEnd
    vgaHSync = activeLow $ pure hSyncStart .<=. hCount .&&. hCount .<. pure hSyncEnd
    vgaStartLine = hCount .==. pure hSyncStart
    vgaStartFrame = vgaStartLine .&&. vCount .==. pure vSyncStart
    vgaX = enable <$> (pure (hSyncEnd + hPost) .<. hCount) <*> (hCount - pure (hSyncEnd + hPost))
    vgaY = enable <$> (vCount .<. pure vSize) <*> vCount

    endLine = hCount .==. pure hMax
    endFrame = vCount .==. pure vMax
    hCount = register (0 :: Unsigned w) $ mux endLine 0 (hCount + 1)
    vCount = regEn (0 :: Unsigned h) endLine $ mux endFrame 0 (vCount + 1)

    VGATiming hSize hPre hSync hPost = vgaHorizTiming
    hSyncStart = 0
    hSyncEnd = hSyncStart + hSync
    hMax = sum [hSync, hPost, hSize, hPre] - 1

    VGATiming vSize vPre vSync vPost = vgaVertTiming
    vSyncStart = vSize + vPre
    vSyncEnd = vSyncStart + vSync
    vMax = sum [vSize, vPre, vSync, vPost] - 1

-- | VGA 640*480@60Hz, 25.175 MHz pixel clock
vga640x480at60 :: VGATimings 10 10
vga640x480at60 = VGATimings
    { vgaHorizTiming = VGATiming 640 16 96 48
    , vgaVertTiming  = VGATiming 480 11  2 31
    }

-- | VGA 800x600@72Hz, 50 MHz pixel clock
vga800x600at72 :: VGATimings 11 10
vga800x600at72 = VGATimings
    { vgaHorizTiming = VGATiming 800 56 120 64
    , vgaVertTiming  = VGATiming 600 37   6 23
    }

-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga800x600at60 :: VGATimings 11 10
vga800x600at60 = VGATimings
    { vgaHorizTiming = VGATiming 800 40 128 88
    , vgaVertTiming  = VGATiming 600  1   4 23
    }

-- | VGA 1024*768@60Hz, 65 MHz pixel clock
vga1024x768at60 :: VGATimings 11 10
vga1024x768at60 = VGATimings
    { vgaHorizTiming = VGATiming 1024 24 136 160
    , vgaVertTiming  = VGATiming  768  3   6  29
    }
