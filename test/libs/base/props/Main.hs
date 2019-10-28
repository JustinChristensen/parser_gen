module Main where

import Control.Monad (unless)
import System.Exit
import qualified IntSetProps

main :: IO ()
main = do
    passed <- IntSetProps.runTests
    unless passed exitFailure
