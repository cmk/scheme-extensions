{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Fixed (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Functor.Fixed
import Data.Functor.Foldable
import Data.Functor.Pattern

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

forAllL :: Gen (Mu (Cons Int)) -> PropertyT IO (Mu (Cons Int))
forAllL = forAllWith (show . toList)

genList :: Gen (Mu (Cons Int))
genList = fromList <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))

---------------------------------------------------------------------
-- Lambek round-trips
---------------------------------------------------------------------

prop_wrap_unwrap :: Property
prop_wrap_unwrap = property $ do
    xs <- forAllL genList
    toList (wrap (unwrap xs)) === toList xs

---------------------------------------------------------------------
-- Fold laws
---------------------------------------------------------------------

prop_fold_fusion :: Property
prop_fold_fusion = property $ do
    xs <- forAllL genList
    let alg :: Cons Int Int -> Int
        alg Nil = 0
        alg (Cons a n) = a + n
    fold alg (wrap (unwrap xs)) === alg (fmap (fold alg) (unwrap xs))

prop_fold_wrap_id :: Property
prop_fold_wrap_id = property $ do
    xs <- forAllL genList
    toList (fold wrap xs) === toList xs

---------------------------------------------------------------------
-- Hoist laws
---------------------------------------------------------------------

prop_hoist_id :: Property
prop_hoist_id = property $ do
    xs <- forAllL genList
    toList (hoistMu id xs) === toList xs

prop_hoist_compose :: Property
prop_hoist_compose = property $ do
    xs <- forAllL genList
    let n :: Cons Int r -> Cons Int r
        n Nil = Nil
        n (Cons a r) = Cons (a + 1) r
        m :: Cons Int r -> Cons Int r
        m Nil = Nil
        m (Cons a r) = Cons (a * 2) r
    toList (hoistMu (n . m) xs) === toList (hoistMu n (hoistMu m xs))

---------------------------------------------------------------------
-- Refold coherence
---------------------------------------------------------------------

prop_refold_coherence :: Property
prop_refold_coherence = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
    let alg :: Cons Int Int -> Int
        alg Nil = 0
        alg (Cons a n) = a + n
        coalg :: [Int] -> Cons Int [Int]
        coalg [] = Nil
        coalg (a : as) = Cons a as
    refold alg coalg xs === fold alg (unfold coalg xs)

---------------------------------------------------------------------
-- Pair
---------------------------------------------------------------------

prop_pair_swap_involution :: Property
prop_pair_swap_involution = property $ do
    a <- forAll $ Gen.int (Range.linear 0 100)
    b <- forAll $ Gen.int (Range.linear 0 100)
    swapPair (swapPair (a :!: b)) === (a :!: b)

prop_zip_algebras :: Property
prop_zip_algebras = property $ do
    xs <- forAllL genList
    let sumAlg :: Cons Int Int -> Int
        sumAlg Nil = 0
        sumAlg (Cons a b) = a + b
        lenAlg :: Cons Int Int -> Int
        lenAlg Nil = 0
        lenAlg (Cons _ b) = b + 1
        pair = fold (zipAlgebras sumAlg lenAlg) xs
    pairFst pair === sum (toList xs)
    pairSnd pair === length (toList xs)
