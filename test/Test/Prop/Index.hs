{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Index (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Functor.Index
import Data.Word (Word8, Word16, Word32, Word64)

tests :: IO Bool
tests = checkParallel $$(discover)

prop_bits8_roundtrip :: Property
prop_bits8_roundtrip = property $ do
    w <- forAll $ Gen.word8 Range.linearBounded
    fromBits8 (toBits8 w) === w

prop_bits16_roundtrip :: Property
prop_bits16_roundtrip = property $ do
    w <- forAll $ Gen.word16 Range.linearBounded
    fromBits16 (toBits16 w) === w

prop_bits32_roundtrip :: Property
prop_bits32_roundtrip = property $ do
    w <- forAll $ Gen.word32 Range.linearBounded
    fromBits32 (toBits32 w) === w

prop_bits64_roundtrip :: Property
prop_bits64_roundtrip = property $ do
    w <- forAll $ Gen.word64 Range.linearBounded
    fromBits64 (toBits64 w) === w

prop_bits8_all_true :: Property
prop_bits8_all_true = property $ do
    fromBits8 (const True) === (255 :: Word8)

prop_bits8_all_false :: Property
prop_bits8_all_false = property $ do
    fromBits8 (const False) === (0 :: Word8)
