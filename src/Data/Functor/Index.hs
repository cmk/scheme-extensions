{-# LANGUAGE DeriveGeneric #-}

-- | Fixed-size index types for 'Representable' functors.
--
-- Each @IN@ type has exactly @N@ inhabitants, making @(->) IN@
-- a 'Representable' functor with @Rep = IN@. This gives
-- cotraversals\/grates over fixed-size types:
--
-- @
-- Word8  ≅  I8  -> Bool   (8 bits)
-- Word16 ≅  I16 -> Bool   (16 bits)
-- Word32 ≅  I32 -> Bool   (32 bits)
-- Word64 ≅  I64 -> Bool   (64 bits)
-- @
--
-- The isos 'toBits8'\/'fromBits8' etc. witness these equivalences.
-- Compose with the @(->) IN@ cotraversal to get bit-level access.
module Data.Functor.Index (
    -- * Index types
    I4 (..),
    I8 (..),
    I16 (..),
    I32 (..),
    I64 (..),

    -- * Word ≅ (IN -> Bool) isos
    toBits8,
    fromBits8,
    toBits16,
    fromBits16,
    toBits32,
    fromBits32,
    toBits64,
    fromBits64,
) where

import Data.Bits (testBit, setBit)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

---------------------------------------------------------------------
-- Index types
---------------------------------------------------------------------

-- | 4-element index type. Represents a nibble (half-byte).
data I4 = I41 | I42 | I43 | I44
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | 8-element index type. Represents bit positions in a 'Word8'.
data I8 = I81 | I82 | I83 | I84 | I85 | I86 | I87 | I88
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | 16-element index type. Represents bit positions in a 'Word16'.
data I16
    = I161  | I162  | I163  | I164
    | I165  | I166  | I167  | I168
    | I169  | I1610 | I1611 | I1612
    | I1613 | I1614 | I1615 | I1616
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | 32-element index type. Represents bit positions in a 'Word32'.
data I32
    = I321  | I322  | I323  | I324
    | I325  | I326  | I327  | I328
    | I329  | I3210 | I3211 | I3212
    | I3213 | I3214 | I3215 | I3216
    | I3217 | I3218 | I3219 | I3220
    | I3221 | I3222 | I3223 | I3224
    | I3225 | I3226 | I3227 | I3228
    | I3229 | I3230 | I3231 | I3232
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | 64-element index type. Represents bit positions in a 'Word64'.
data I64
    = I641  | I642  | I643  | I644
    | I645  | I646  | I647  | I648
    | I649  | I6410 | I6411 | I6412
    | I6413 | I6414 | I6415 | I6416
    | I6417 | I6418 | I6419 | I6420
    | I6421 | I6422 | I6423 | I6424
    | I6425 | I6426 | I6427 | I6428
    | I6429 | I6430 | I6431 | I6432
    | I6433 | I6434 | I6435 | I6436
    | I6437 | I6438 | I6439 | I6440
    | I6441 | I6442 | I6443 | I6444
    | I6445 | I6446 | I6447 | I6448
    | I6449 | I6450 | I6451 | I6452
    | I6453 | I6454 | I6455 | I6456
    | I6457 | I6458 | I6459 | I6460
    | I6461 | I6462 | I6463 | I6464
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

---------------------------------------------------------------------
-- Word ≅ (IN -> Bool) isos
---------------------------------------------------------------------

-- | Decompose a 'Word8' into its 8 bits.
--
-- @toBits8 w i = testBit w (fromEnum i)@
--
-- >>> toBits8 0xFF I1
-- True
-- >>> toBits8 0x00 I1
-- False
{-# INLINE toBits8 #-}
toBits8 :: Word8 -> I8 -> Bool
toBits8 w i = testBit w (fromEnum i)

-- | Reconstruct a 'Word8' from 8 bits.
--
-- @fromBits8 . toBits8 = id@
-- @toBits8 . fromBits8 = id@
--
-- >>> fromBits8 (const True)
-- 255
-- >>> fromBits8 (const False)
-- 0
{-# INLINE fromBits8 #-}
fromBits8 :: (I8 -> Bool) -> Word8
fromBits8 f = foldl' go 0 [minBound .. maxBound]
  where
    go acc i = if f i then setBit acc (fromEnum i) else acc

-- | Decompose a 'Word16' into its 16 bits.
{-# INLINE toBits16 #-}
toBits16 :: Word16 -> I16 -> Bool
toBits16 w i = testBit w (fromEnum i)

-- | Reconstruct a 'Word16' from 16 bits.
{-# INLINE fromBits16 #-}
fromBits16 :: (I16 -> Bool) -> Word16
fromBits16 f = foldl' go 0 [minBound .. maxBound]
  where
    go acc i = if f i then setBit acc (fromEnum i) else acc

-- | Decompose a 'Word32' into its 32 bits.
{-# INLINE toBits32 #-}
toBits32 :: Word32 -> I32 -> Bool
toBits32 w i = testBit w (fromEnum i)

-- | Reconstruct a 'Word32' from 32 bits.
{-# INLINE fromBits32 #-}
fromBits32 :: (I32 -> Bool) -> Word32
fromBits32 f = foldl' go 0 [minBound .. maxBound]
  where
    go acc i = if f i then setBit acc (fromEnum i) else acc

-- | Decompose a 'Word64' into its 64 bits.
{-# INLINE toBits64 #-}
toBits64 :: Word64 -> I64 -> Bool
toBits64 w i = testBit w (fromEnum i)

-- | Reconstruct a 'Word64' from 64 bits.
{-# INLINE fromBits64 #-}
fromBits64 :: (I64 -> Bool) -> Word64
fromBits64 f = foldl' go 0 [minBound .. maxBound]
  where
    go acc i = if f i then setBit acc (fromEnum i) else acc

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x : xs) = let z' = f z x in z' `seq` foldl' f z' xs
