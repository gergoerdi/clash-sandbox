{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Cactus.Clash.SerialRX
    ( RXState(..)
    , rx
    ) where

import Clash.Prelude
import Cactus.Clash.Util

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import Control.Category ((>>>))
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Writer
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Monoid

data RXState = RXState
    { buf1, buf2 :: Bit
    , cnt :: Word16
    , byte :: Vec 8 Bit
    , state :: MicroState
    }
    deriving (Generic, NFData, Show)

data MicroState
    = RXIdle
    | RXStart
    | RXBit (Index 8)
    | RXStop
    | RXCleanup
    deriving (Generic, NFData, Show)

rotateLeftS' :: forall a d. (BitPack a, KnownNat (BitSize a)) => SNat d -> a -> a
rotateLeftS' d x = unpack . pack $ rotateLeftS (unpack . pack $ x :: Vec (BitSize a) Bit) d

rx0 :: Word16 -> Bit -> State RXState (Maybe Word8)
rx0 divider bit = do
    s@RXState{..} <- get
    modify $ \s -> s{ buf2 = buf1, buf1 = bit, cnt = cnt - 1 }

    fmap getLast $ execWriterT $ case state of
        RXIdle -> do
            when (buf2 == low) $ goto RXStart
        RXStart -> when (cnt == divider `div` 2) $ do
            goto $ if buf2 == low then RXBit maxBound else RXIdle
        RXBit i -> when (cnt == 0) $ do
            modify $ \s -> s{ byte = replace i buf2 byte }
            goto $ maybe RXStop RXBit $ predIdx i
        RXStop -> when (cnt == 0) $ do
            tell $ Last . Just . unpack . pack $ byte
            goto RXCleanup
        RXCleanup -> goto RXIdle
  where
    goto st = modify $ \s -> s{ cnt = divider, state = st }

rx
    :: (HiddenClockReset domain gated synchronous)
    => Word32
    -> Word32
    -> Signal domain Bit
    -> Signal domain (Maybe Word8)
rx clkRate serialRate = mealyState (rx0 $ fromIntegral $ clkRate `div` serialRate) s0
  where
    s0 = RXState
        { buf1 = 0
        , buf2 = 0
        , cnt = 0
        , byte = pure 0
        , state = RXIdle
        }
