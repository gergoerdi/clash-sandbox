{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Brainfuck.CPU where

import Clash.Prelude
import Cactus.Clash.Util

import Control.Monad.State
import Data.Word
import Data.Bits
import Data.Char (chr)
import Data.Maybe (fromMaybe)

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

data MicroState
    = WaitMem
    | Exec
    | Skip (Unsigned 5)
    | WaitOutput
    | WaitInput
    | Halt

data CPUState = CPUState
    { cpuPC :: PC
    , cpuPtr :: Ptr
    , cpuStack :: Vec 16 PC
    , cpuSP :: Index 16
    , cpuState :: MicroState
    }

cpuState0 :: CPUState
cpuState0 = CPUState
    { cpuPC = 0
    , cpuPtr = 0
    , cpuStack = repeat 0
    , cpuSP = 0
    , cpuState = WaitMem
    }

data CPUOut = CPUOut
    { cpuOutPC :: PC
    , cpuOutPtr :: Ptr
    , cpuOutWrite :: Maybe Word8
    , cpuOutOutput :: Maybe Word8
    , cpuOutNeedInput :: Bool
    }

{-# INLINE fetch #-}
fetch :: CPUIn -> State CPUState (Maybe BF)
fetch CPUIn{..} = do
    modify $ \s -> s{ cpuPC = cpuPC s + 1}
    return $ decode cpuInProg

{-# INLINE push #-}
push :: PC -> State CPUState ()
push pc = do
    modify $ \s@CPUState{..} -> s{ cpuSP = nextIdx cpuSP, cpuStack = replace cpuSP pc cpuStack }

{-# INLINE pop #-}
pop :: State CPUState PC
pop = do
    modify $ \s@CPUState{..} -> s{ cpuSP = prevIdx cpuSP }
    CPUState{..} <- get
    return $ cpuStack !! cpuSP

stepCPU :: CPUIn -> State CPUState CPUOut
stepCPU cpuIn@CPUIn{..} = do
    s <- get
    (cpuOutWrite, cpuOutOutput, cpuOutNeedInput) <-
        case cpuState s of
            WaitMem -> do
                modify $ \s -> s{ cpuState = Exec }
                return (Nothing, Nothing, False)
            Halt -> do
                return (Nothing, Nothing, False)
            Skip n -> do
                fetch cpuIn >>= \case
                    Just StartLoop -> modify $ \s ->
                      s{ cpuState = Skip (n + 1)
                       }
                    Just EndLoop -> modify $ \s ->
                      s{ cpuState = if n == 0 then Exec else Skip (n - 1)
                       }
                    _ -> return ()
                return (Nothing, Nothing, False)
            WaitOutput -> do
                when cpuInOutputAck $ modify $ \s -> s{ cpuState = Exec }
                return (Nothing, Just cpuInRead, False)
            WaitInput -> case cpuInInput of
                Nothing -> return (Nothing, Nothing, True)
                Just input -> do
                    modify $ \s -> s{ cpuState = Exec }
                    return (Just input, Nothing, False)
            Exec -> do
                fetch cpuIn >>= \case
                    Nothing -> do
                        return (Nothing, Nothing, False)
                    Just IncPtr -> do
                        modify $ \s -> s{ cpuPtr = cpuPtr s + 1 }
                        return (Nothing, Nothing, False)
                    Just DecPtr -> do
                        modify $ \s -> s{ cpuPtr = cpuPtr s - 1 }
                        return (Nothing, Nothing, False)
                    Just IncCell -> do
                        modify $ \s -> s{ cpuState = WaitMem }
                        return (Just (cpuInRead + 1), Nothing, False)
                    Just DecCell -> do
                        modify $ \s -> s{ cpuState = WaitMem }
                        return (Just (cpuInRead - 1), Nothing, False)
                    Just Output -> do
                        modify $ \s -> s{ cpuState = WaitOutput }
                        return (Nothing, Just cpuInRead, False)
                    Just Input -> do
                        modify $ \s -> s{ cpuState = WaitInput }
                        return (Nothing, Nothing, True)
                    Just StartLoop
                      | cpuInRead == 0 -> do
                        modify $ \s -> s{ cpuState = Skip 0 }
                        return (Nothing, Nothing, False)
                      | otherwise -> do
                        push $ cpuPC s
                        return (Nothing, Nothing, False)
                    Just EndLoop
                      | cpuInRead == 0 -> do
                        return (Nothing, Nothing, False)
                      | otherwise -> do
                        pc <- pop
                        modify $ \s -> s{ cpuPC = pc }
                        return (Nothing, Nothing, False)
                    Just NULL -> do
                        modify $ \s -> s{ cpuState = Halt }
                        return (Nothing, Nothing, False)
    CPUState{..} <- get
    return CPUOut
        { cpuOutPC = cpuPC
        , cpuOutPtr = cpuPtr
        , ..
        }

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