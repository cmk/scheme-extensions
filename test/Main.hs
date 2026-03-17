module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import qualified Test.Prop.Fixed as Fixed
import qualified Test.Prop.Foldable as Foldable
import qualified Test.Prop.Pattern as Pattern
import qualified Test.Prop.Index as Index

main :: IO ()
main = do
    ok <- and <$> sequence
        [ Fixed.tests
        , Foldable.tests
        , Pattern.tests
        , Index.tests
        ]
    if ok then exitSuccess else exitFailure
