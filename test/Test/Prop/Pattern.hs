{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Pattern (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Functor.Fixed
import Data.Functor.Foldable
import Data.Functor.Pattern

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Cons combinators
---------------------------------------------------------------------

prop_list_roundtrip :: Property
prop_list_roundtrip = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
    toList (fromList xs) === xs

prop_elim_nil :: Property
prop_elim_nil = property $ do
    elim (0 :: Int) (\_ _ -> 1) Nil === 0

prop_elim_cons :: Property
prop_elim_cons = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    m <- forAll $ Gen.int (Range.linear 0 100)
    elim 0 (+) (Cons n m) === n + m

prop_toCons_fromCons :: Property
prop_toCons_fromCons = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    m <- forAll $ Gen.int (Range.linear 0 100)
    toCons (fromCons (Cons n m)) === Cons n m

prop_fromCons_nil :: Property
prop_fromCons_nil = property $ do
    fromCons (Nil :: Cons Int Int) === Nothing

prop_swap_involution :: Property
prop_swap_involution = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    m <- forAll $ Gen.int (Range.linear 0 100)
    swapCons (swapCons (Cons n m)) === Cons n m

prop_cons_semigroup :: Property
prop_cons_semigroup = property $ do
    let a = Cons [1 :: Int] [2]
        b = Cons [3] [4]
    a <> b === Cons [1, 3] [2, 4]

prop_cons_monoid :: Property
prop_cons_monoid = property $ do
    let a = Cons [1 :: Int] [2]
    (mempty <> a) === a
    (a <> mempty) === a
