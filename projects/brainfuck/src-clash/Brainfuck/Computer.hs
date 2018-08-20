{-# LANGUAGE RecordWildCards, TupleSections #-}
module Brainfuck.Computer where

import Clash.Prelude
import Cactus.Clash.Util
import Data.Word

import Brainfuck.CPU

computer
    :: (HiddenClockReset domain gated synchronous)
    => FilePath
    -> Signal domain (Maybe Word8)
    -> Signal domain Bool
    -> (Signal domain (Maybe Word8), Signal domain Bool)
computer romFile input outputAck = (output, needInput)
  where
    cpuOut = mealyState stepCPU cpuState0 cpuIn
    cpuIn = CPUIn <$> romOut <*> ramOut <*> outputAck <*> input

    romOut = unpack <$> blockRamFile d1024 romFile (cpuOutPC <$> cpuOut) (pure Nothing)

    packWrite CPUOut{..} = (cpuOutPtr,) <$> cpuOutWrite
    ramOut = blockRam (replicate d1024 0) (cpuOutPtr <$> cpuOut) (packWrite <$> cpuOut)

    output = cpuOutOutput <$> cpuOut
    needInput = cpuOutNeedInput <$> cpuOut

showPC :: PC -> Word8
showPC x = lo
  where
    hi :: Unsigned 2
    lo :: Word8
    (hi, lo) = unpack . pack $ x
