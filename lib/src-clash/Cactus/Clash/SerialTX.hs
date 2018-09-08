{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
module Cactus.Clash.SerialTX
    ( TXOut(..)
    , tx
    , fifo
    ) where

import Clash.Prelude
import Cactus.Clash.Util

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe

data TXState = TXIdle
             | TXStart
             | TXBit (Index 8)

data TXOut dom = TXOut{ txReady :: Signal dom Bool, txOut :: Signal dom Bit }

rotateRightS' :: forall a d. (BitPack a, KnownNat (BitSize a)) => SNat d -> a -> a
rotateRightS' d x = unpack . pack $ rotateRightS (unpack . pack $ x :: Vec (BitSize a) Bit) d

tx0 :: Word32 -> Maybe Word8 -> State (Word32, Maybe (Word8, TXState)) (Bool, Bit)
tx0 divider v = do
    (cnt, s) <- get
    case s of
        Nothing -> do
            traverse (\v -> put (divider, Just (v, TXIdle))) v
            return (True, high)
        Just (v, s') -> (False,) <$> case s' of
            TXIdle -> do
                nextState $ Just (v, TXStart)
                return high
            TXStart -> do
                nextState $ Just (v, TXBit 0)
                return low
            TXBit i -> do
                let v' = rotateRightS' d1 v
                nextState $ ((v',) . TXBit) <$> succIdx i
                return $ lsb . pack $ v
  where
    nextState s = do
        (cnt, s0) <- get
        put $ if cnt == 0 then (divider, s) else (cnt - 1, s0)

tx
    :: (HiddenClockReset domain gated synchronous)
    => Word32
    -> Word32
    -> Signal domain (Maybe Word8)
    -> TXOut domain
tx clkRate serialRate inp = TXOut{..}
  where
    (txReady, txOut) = unbundle $ mealyState (tx0 $ clkRate `div` serialRate) (0, Nothing) inp

fifo
    :: forall domain gated synchronous a. (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe a) -> Signal domain Bool -> (Signal domain (Maybe a), Signal domain Bool)
fifo input consumed = unbundle $ mealyState step Nothing $ bundle (input, consumed)
  where
    step (input, consumed) = do
        if consumed then case input of
            Nothing -> return (Nothing, True)
            Just x -> do
                put $ Just x
                return (Just x, False)
          else do
            x <- get
            return (x, isNothing x)
