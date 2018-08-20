{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
module Cactus.Clash.SerialTX
    ( TXOut(..)
    , tx
    ) where

import Clash.Prelude
import Cactus.Clash.Util

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits

data TXState = TXIdle
             | TXStart
             | TXBit (Index 8)

data TXOut = TXOut{ txReady :: Bool, txOut :: Bit }

serialRate :: Word32
serialRate = 9600

rotateRightS' :: forall a d. (BitPack a, KnownNat (BitSize a)) => SNat d -> a -> a
rotateRightS' d x = unpack . pack $ rotateRightS (unpack . pack $ x :: Vec (BitSize a) Bit) d

tx0 :: Maybe Word8 -> State (Maybe (Word8, TXState)) TXOut
tx0 v = do
    s <- get
    case s of
        Nothing -> do
            traverse (\v -> put $ Just (v, TXIdle)) v
            return $ TXOut True high
        Just (v, s) -> TXOut False <$> case s of
            TXIdle -> do
                put $ Just (v, TXStart)
                return high
            TXStart -> do
                put $ Just (v, TXBit 0)
                return low
            TXBit i -> do
                let v' = rotateRightS' d1 v
                put $ ((v',) . TXBit) <$> succIdx i
                return $ lsb . pack $ v

stepped :: Bool -> State s a -> State s a
stepped allow act = do
    s0 <- get
    res <- act
    unless allow $ put s0
    return res

tx
    :: (HiddenClockReset domain gated synchronous)
    => Word32
    -> Word32
    -> Signal domain (Maybe Word8)
    -> Signal domain TXOut
tx clkRate serialRate = mealyState' tx0 Nothing (countTo $ clkRate `div` serialRate)
