{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Foldable (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude hiding (iterate, repeat)

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
-- Elgot
---------------------------------------------------------------------

prop_elgot_early :: Property
prop_elgot_early = property $ do
    let alg :: Cons Int [Int] -> [Int]
        alg Nil = []
        alg (Cons a bs) = a : bs
        coalg :: [Int] -> Either [Int] (Cons Int [Int])
        coalg [] = Left []
        coalg (x : xs)
            | x > 3 = Left []
            | otherwise = Right (Cons x xs)
    elgot alg coalg [1, 2, 3, 4, 5] === [1, 2, 3]

---------------------------------------------------------------------
-- Prepro / postpro
---------------------------------------------------------------------

prop_prepro_id :: Property
prop_prepro_id = property $ do
    xs <- forAllL genList
    let alg :: Cons Int Int -> Int
        alg Nil = 0
        alg (Cons a b) = a + b
    prepro id alg xs === fold alg xs

prop_postpro_id :: Property
prop_postpro_id = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
    let coalg :: [Int] -> Cons Int [Int]
        coalg [] = Nil
        coalg (a : as) = Cons a as
    toList (postpro id coalg xs) === xs

---------------------------------------------------------------------
-- foldM
---------------------------------------------------------------------

prop_foldM_pure :: Property
prop_foldM_pure = property $ do
    xs <- forAllL genList
    let alg :: Cons Int Int -> Int
        alg Nil = 0
        alg (Cons a b) = a + b
    foldM (pure . alg) xs === (Just (fold alg xs) :: Maybe Int)

---------------------------------------------------------------------
-- Streaming (fstream)
---------------------------------------------------------------------

prop_fstream_id :: Property
prop_fstream_id = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
    let input = fromList xs
        flush = \case [] -> Nil; (a : as) -> Cons a as
        result = fstream unwrap wrap flush (\acc b -> acc ++ [b]) flush [] input
    toList result === xs

---------------------------------------------------------------------
-- Nu iteration
---------------------------------------------------------------------

prop_iterate :: Property
prop_iterate = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let nu = iterate (+ 1) n
        take5 (Nu step seed) = take 5 (go seed)
          where go s = case step s of
                    Nil -> []
                    Cons a s' -> a : go s'
    take5 nu === [n, n+1, n+2, n+3, n+4]

prop_repeat :: Property
prop_repeat = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let nu = repeat n
        take5 (Nu step seed) = take 5 (go seed)
          where go s = case step s of
                    Nil -> []
                    Cons a s' -> a : go s'
    take5 nu === [n, n, n, n, n]
