{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Brainfuck.CPU where

import Clash.Prelude
import Cactus.Clash.Util

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import Control.Monad.Trans.Class as T
import Control.Monad.State
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Writer
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

data W = W{ wOutput :: Last Word8
          , wWrite :: Last (Ptr, Word8)
          -- , wInput :: Any
          }
instance Semigroup W where
    W o1 w1 <> W o2 w2 = W (o1 <> o2) (w1 <> w2)

instance Monoid W where
    mempty = W mempty mempty

cpuOut :: CPUState -> W -> CPUOut
cpuOut CPUState{..} W{..} = CPUOut
    { cpuOutPC = cpuPC
    , cpuOutPtr = cpuPtr
    , cpuOutWrite = getLast wWrite
    , cpuOutOutput = getLast wOutput
    , cpuOutNeedInput = cpuState == WaitInput
    }

type CPU r = ContT r (WriterT W (State CPUState))

stepCPU' :: CPUIn -> CPU () ()
stepCPU' cpuIn@CPUIn{..} = resetT $ do
    wait
    s <- get
    case cpuState s of
        Halt -> return ()
        Skip n -> do
            fetch cpuIn >>= \op -> case op of
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
        Exec -> fetch cpuIn >>= \op -> case op of
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
    break = shiftT $ \k -> return ()

    fetch CPUIn{..} = do
        modify $ \s -> s{ cpuPC = cpuPC s + 1 }
        maybe break return $ decode cpuInProg

    goto st = modify $ \s -> s{ cpuState = st }
    jump pc = modify $ \s -> s{ cpuPC = pc }
    modifyPtr f = modify $ \s -> s{ cpuPtr = f $ cpuPtr s }

    wait = do
        waiting <- gets cpuWaitMem
        when waiting $ do
            modify $ \s -> s{ cpuWaitMem = False}
            break

    write addr x = do
        modify $ \s -> s{ cpuWaitMem = True }
        T.lift $ tell $ mempty{ wWrite = Last . Just $ (addr, x) }

    output x = do
        T.lift $ tell $ mempty{ wOutput = Last . Just $ x }
        goto WaitOutput

stepCPU :: CPUIn -> State CPUState (CPUState, CPUOut)
stepCPU cpuIn = do
    s0 <- get
    w <- execWriterT . evalContT $ stepCPU' cpuIn
    s <- get
    return (s0, cpuOut s w)

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
