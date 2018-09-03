module Cactus.Clash.Util
    ( mealyState
    , activeLowReset
    , activeLow
    , activeHigh
    , countTo
    , countWhen
    , muxRR
    , nextIdx
    , prevIdx
    , succIdx
    , predIdx
    , debounce
    , diff
    , enable
    , extremum
    , regShiftIn
    ) where

import Clash.Prelude
import Control.Monad.State
import Data.Word
import Data.Maybe (fromMaybe, isJust)

mealyState :: (HiddenClockReset domain gated synchronous)
           => (i -> State s o) -> s -> (Signal domain i -> Signal domain o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s
               in (s', y)

activeLowReset :: Reset domain Asynchronous -> Reset domain Asynchronous
activeLowReset = unsafeToAsyncReset . (not <$>) . unsafeFromAsyncReset

activeLow :: (Functor f) => f Bool -> f Bit
activeLow = fmap complement . activeHigh

activeHigh :: (Functor f) => f Bool -> f Bit
activeHigh = fmap boolToBit

countWhen
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain Bool -> Signal domain Word16
countWhen = moore step id 0
  where
    step n b = if b then succ n else n

diff
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe a) -> Signal domain (Maybe a)
diff = mealy step False
  where
    step False new = (isJust new, new)
    step True new = (isJust new, Nothing)

countTo
    :: (HiddenClockReset domain gated synchronous)
    => Word32 -> Signal domain Bool
countTo n = mealyState step 0 (pure ())
  where
    step () = do
        k <- get
        let k' = k + 1
            finished = k' == n
        put $ if finished then 0 else k'
        return finished

muxRR
    :: forall domain gated synchronous n a. (HiddenClockReset domain gated synchronous, KnownNat n)
    => Signal domain Bool
    -> Vec n (Signal domain a)
    -> (Signal domain (Vec n Bool), Signal domain a)
muxRR next ss = let (mask, i) = unbundle $ moore step id (mask0, (0 :: Index n)) next
                in (mask, (!!) <$> bundle ss <*> i)
  where
    step s False = s
    step (mask, i) True = (rotateLeftS mask d1, nextIdx i)

    mask0 = repeat False <<+ True

nextIdx :: (KnownNat n) => Index n -> Index n
nextIdx = fromMaybe 0 . succIdx

prevIdx :: (KnownNat n) => Index n -> Index n
prevIdx = fromMaybe maxBound . predIdx

succIdx :: (KnownNat n) => Index n -> Maybe (Index n)
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

predIdx :: (KnownNat n) => Index n -> Maybe (Index n)
predIdx x | x == 0 = Nothing
          | otherwise = Just $ pred x

debounce
    :: forall domain gated synchronous n. (HiddenClockReset domain gated synchronous, KnownNat n)
    => SNat n -> Signal domain Bool -> (Signal domain (Maybe Bool))
debounce n = mealyState step (0 :: Unsigned n, False)
  where
    step button = do
        (counter, prev) <- get
        let stable = counter == maxBound
            changing = prev /= button

        let counter'
              | changing  = 0
              | stable    = maxBound
              | otherwise = counter + 1
        put (counter', button)

        return $ button <$ guard (not changing && stable)

enable :: Bool -> a -> Maybe a
enable False = const Nothing
enable True = Just

extremum :: (KnownNat n) => Vec n Bit -> Maybe Bit
extremum xs
  | xs == repeat low = Just low
  | xs == repeat high = Just high
  | otherwise = Nothing

regShiftIn
    :: (HiddenClockReset dom gated synchronous, KnownNat n)
    => Vec n a -> Signal dom (Maybe a) -> (Signal dom (Vec n a), Signal dom (Maybe a))
regShiftIn = mealyB $ \xs x -> let out@(xs', _) = shiftIn x xs in (xs', out)
  where
    shiftIn :: (KnownNat n) => Maybe a -> Vec n a -> (Vec n a, Maybe a)
    shiftIn Nothing xs = (xs, Nothing)
    shiftIn (Just x) xs = let (xs', x :> Nil) = shiftInAt0 xs (singleton x)
                          in (xs', Just x)
