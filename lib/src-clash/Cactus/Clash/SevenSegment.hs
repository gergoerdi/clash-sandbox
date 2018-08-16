{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, DeriveFoldable, DeriveTraversable #-}
module Cactus.Clash.SevenSegment
    ( SevenSegment(..)
    , Nybble
    , driveSS
    , dim
    , splitByte
    , encodeHexSS
    , showSS
    ) where

import Clash.Prelude
import Cactus.Clash.Util

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits

type Nybble = Unsigned 4

data SevenSegment n b = SevenSegment
    { ssMask :: Vec n b
    , ssSegments :: Vec 7 b
    , ssDP :: b
    }
deriving instance (KnownNat n, 1 <= n) => Functor (SevenSegment n)
deriving instance (KnownNat n, 1 <= n) => Foldable (SevenSegment n)
deriving instance (KnownNat n, 1 <= n) => Traversable (SevenSegment n)

driveSS
    :: (HiddenClockReset domain gated synchronous, KnownNat n)
    => Word32 -> Signal domain (Vec n Nybble) -> Signal domain (SevenSegment n Bool)
driveSS = driveSS' $ \d -> (encodeHexSS d, False)

driveSS'
    :: (HiddenClockReset domain gated synchronous, KnownNat n)
    => (a -> (Vec 7 Bool, Bool)) -> Word32 -> Signal domain (Vec n a) -> Signal domain (SevenSegment n Bool)
driveSS' toSegments muxSpeed inputs = SevenSegment <$> mask <*> segments <*> dp
  where
    (mask, input) = muxRR (countTo muxSpeed) (unbundle inputs)
    (segments, dp) = unbundle $ toSegments <$> input

dim
    :: (HiddenClockReset domain gated synchronous, KnownNat n)
    => Word32 -> Signal domain (Vec n Bool) -> Signal domain (Vec n Bool)
dim n xs = bundle $ (.&&.) <$> repeat (countTo n) <*> unbundle xs

splitByte :: Word8 -> (Nybble, Nybble)
splitByte = unpack . pack

encodeHexSS :: Nybble -> Vec 7 Bool
encodeHexSS n = case n of
    --                      a      b      c      d      e      f      g
    0x0 -> $(listToVecTH [  True,  True,  True,  True,  True,  True, False ])
    0x1 -> $(listToVecTH [ False,  True,  True, False, False, False, False ])
    0x2 -> $(listToVecTH [  True,  True, False,  True,  True, False,  True ])
    0x3 -> $(listToVecTH [  True,  True,  True,  True, False, False,  True ])
    0x4 -> $(listToVecTH [ False,  True,  True, False, False,  True,  True ])
    0x5 -> $(listToVecTH [  True, False,  True,  True, False,  True,  True ])
    0x6 -> $(listToVecTH [  True, False,  True,  True,  True,  True,  True ])
    0x7 -> $(listToVecTH [  True,  True,  True, False, False, False, False ])
    0x8 -> $(listToVecTH [  True,  True,  True,  True,  True,  True,  True ])
    0x9 -> $(listToVecTH [  True,  True,  True,  True, False,  True,  True ])
    0xa -> $(listToVecTH [  True,  True,  True, False,  True,  True,  True ])
    0xb -> $(listToVecTH [ False, False,  True,  True,  True,  True,  True ])
    0xc -> $(listToVecTH [  True, False, False,  True,  True,  True, False ])
    0xd -> $(listToVecTH [ False,  True,  True,  True,  True, False,  True ])
    0xe -> $(listToVecTH [  True, False, False,  True,  True,  True,  True ])
    0xf -> $(listToVecTH [  True, False, False, False,  True,  True,  True ])

showSS :: Vec 7 Bool -> String
showSS (a :> b :> c :> d :> e :> f :> g :> Nil) = unlines
    [ horiz a
    , vert f b
    , vert f b
    , horiz g
    , vert e c
    , vert e c
    , horiz d
    ]
  where
    horiz :: Bool -> String
    horiz True  = " ### "
    horiz False = "     "

    vert :: Bool -> Bool -> String
    vert b1 b2 = part b1 <> "   " <> part b2
      where
        part True  = "#"
        part False = " "
