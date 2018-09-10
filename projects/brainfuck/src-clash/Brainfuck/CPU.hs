{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Brainfuck.CPU where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.CPU

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import Control.Monad.State
import Control.Monad.Cont
import Data.Word
import Data.Bits
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Monoid

data BF
    = IncPtr
    | DecPtr
    | IncCell
    | DecCell
    | Input
    | Output
    | StartLoop
    | EndLoop
    | NULL

type PC = Unsigned 10
type Ptr = Unsigned 10

data CPUIn = CPUIn
    { cpuInProg :: Word8
    , cpuInRead :: Word8
    , cpuInOutputAck :: Bool
    , cpuInInput :: Maybe Word8
    }
    deriving (Generic, NFData, Show)

data MicroState
    = Exec
    | Skip (Unsigned 5)
    | WaitOutput
    | WaitInput
    | Halt
    deriving (Generic, NFData, Show, Eq)

data CPUState = CPUState
    { cpuPC :: PC
    , cpuPtr :: Ptr
    , cpuStack :: Vec 16 PC
    , cpuSP :: Index 16
    , cpuWaitMem :: Bool
    , cpuState :: MicroState
    }
    deriving (Generic, NFData, Show)

cpuState0 :: CPUState
cpuState0 = CPUState
    { cpuPC = 0
    , cpuPtr = 0
    , cpuStack = repeat 0
    , cpuSP = 0
    , cpuWaitMem = True
    , cpuState = Exec
    }

data CPUOut = CPUOut
    { cpuOutPC :: PC
    , cpuOutPtr :: Ptr
    , cpuOutWrite :: Maybe (Ptr, Word8)
    , cpuOutOutput :: Maybe Word8
    , cpuOutNeedInput :: Bool
    }
    deriving (Generic, Show)

{-# INLINE push #-}
push :: (MonadState CPUState m) => PC -> m ()
push pc = do
    modify $ \s@CPUState{..} -> s{ cpuSP = nextIdx cpuSP, cpuStack = replace cpuSP pc cpuStack }

{-# INLINE pop #-}
pop :: (MonadState CPUState m) => m PC
pop = do
    modify $ \s@CPUState{..} -> s{ cpuSP = prevIdx cpuSP }
    CPUState{..} <- get
    return $ cpuStack !! cpuSP

cpuOut :: CPUState -> CPUOut
cpuOut CPUState{..} = CPUOut
    { cpuOutPC = cpuPC
    , cpuOutPtr = cpuPtr
    , cpuOutWrite = Nothing
    , cpuOutOutput = Nothing
    , cpuOutNeedInput = cpuState == WaitInput
    }

stepCPU :: CPUIn -> State CPUState (CPUState, CPUOut)
stepCPU = runCPUDebug cpuOut $ do
    CPUIn{..} <- input

    s <- get
    case cpuState s of
        _ | cpuWaitMem s -> do
            modify $ \s -> s{ cpuWaitMem = False }
        Halt -> return ()
        Skip n -> do
            fetch $ \op -> case op of
                StartLoop -> goto $ Skip $ n + 1
                EndLoop -> goto $ if n == 0 then Exec else Skip (n - 1)
                _ -> return ()
        WaitOutput -> do
            output cpuInRead
            when cpuInOutputAck $ goto Exec
        WaitInput -> case cpuInInput of
            Nothing -> return ()
            Just input -> do
                goto Exec
                write (cpuPtr s) input
        Exec -> fetch $ \op -> case op of
            IncPtr -> do
                modifyPtr succ
            DecPtr -> do
                modifyPtr pred
            IncCell -> do
                write (cpuPtr s) (cpuInRead + 1)
            DecCell -> do
                write (cpuPtr s) (cpuInRead - 1)
            Output -> do
                output cpuInRead
            Input -> do
                goto WaitInput
            StartLoop
              | cpuInRead == 0 -> do
                  goto $ Skip 0
              | otherwise -> do
                  push $ cpuPC s
            EndLoop -> do
                start <- pop
                when (cpuInRead /= 0) $ jump start
            NULL -> do
                goto Halt
  where
    fetch cb = do
        modify $ \s -> s{ cpuPC = cpuPC s + 1 }
        CPUIn{cpuInProg} <- input
        mapM_ cb $ decode cpuInProg

    goto st = modify $ \s -> s{ cpuState = st }
    jump pc = modify $ \s -> s{ cpuPC = pc }
    modifyPtr f = modify $ \s -> s{ cpuPtr = f $ cpuPtr s }

    write addr x = do
        modify $ \s -> s{ cpuWaitMem = True }
        tell $ \out -> out{ cpuOutWrite = Just (addr, x) }

    output x = do
        tell $ \out -> out{ cpuOutOutput = Just x }
        goto WaitOutput

ascii :: Word8 -> Char
ascii = chr . fromIntegral

decode :: Word8 -> Maybe BF
decode (ascii -> '>') = Just IncPtr
decode (ascii -> '<') = Just DecPtr
decode (ascii -> '+') = Just IncCell
decode (ascii -> '-') = Just DecCell
decode (ascii -> ',') = Just Input
decode (ascii -> '.') = Just Output
decode (ascii -> '[') = Just StartLoop
decode (ascii -> ']') = Just EndLoop
decode 0x00 = Just NULL
decode _ = Nothing
