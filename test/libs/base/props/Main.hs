module Main where

import Control.Monad (unless)
import System.Exit
import qualified CIntSetProps

main :: IO ()
main = do
    passed <- CIntSetProps.runTests
    unless passed exitFailure
