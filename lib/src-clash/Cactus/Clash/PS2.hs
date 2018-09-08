{-# LANGUAGE RecordWildCards #-}
module Cactus.Clash.PS2 (PS2(..), samplePS2, decodePS2) where

import Clash.Prelude
import Cactus.Clash.Util
import Data.Word
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Monoid

data PS2 dom = PS2
    { ps2Clk :: Signal dom Bit
    , ps2Data :: Signal dom Bit
    }

samplePS2
    :: (HiddenClockReset dom gated synchronous)
    => PS2 dom -> Signal dom (Maybe Bit)
samplePS2 PS2{..} = enable <$> isFalling low ps2Clk' <*> ps2Data'
  where
    ps2Clk' = debounce d3 low ps2Clk
    ps2Data' = debounce d3 low ps2Data

data PS2State
    = Idle
    | Bit Word8 (Index 8)
    | Parity Word8
    | Stop (Maybe Word8)
    deriving (Show, Eq)

decodePS2
    :: (HiddenClockReset dom gated synchronous)
    => Signal dom (Maybe Bit) -> Signal dom (Maybe Word8)
decodePS2 = flip mealyState Idle $ \bit -> fmap getLast . execWriterT . forM_ bit $ \bit -> do
    state <- get
    case state of
        Idle -> do
            when (bit == low) $ put $ Bit 0 0
        Bit x i -> do
            let x' = shiftInLeft bit x
            put $ maybe (Parity x') (Bit x') $ succIdx i
        Parity x -> do
            let checked = bitToBool bit `xor` parity x
            put $ Stop $ enable checked x
        Stop x -> do
            when (bit == high) $ tell $ Last x
            put Idle
