module Main where

import Utils
import Control.Monad (unless)
import Data.Function
import System.Exit
import Test.QuickCheck hiding (verbose)
import qualified CIntSetProps
import qualified CHashTableProps
import qualified CRBTreeProps

main :: IO ()
main = do
        verbose <- readEnv "VERBOSE" False
        size <- readEnv "SIZE" 30
        tests <- readEnv "TESTS" 100
        let runProps = propRunner verbose (args size tests)
        passed <- and <$> sequence (fmap (runProps &) suites)
        unless passed exitFailure
    where
        propRunner True = verboseCheckWithResult
        propRunner False = quickCheckWithResult
        args size tests = stdArgs { maxSize = size, maxSuccess = tests }
        suites = [
                CIntSetProps.runTests,
                CHashTableProps.runTests,
                CRBTreeProps.runTests
            ]
