module Main where

import Control.Monad (unless)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Function
import System.Exit
import Test.QuickCheck hiding (verbose)
import qualified CIntSetProps

readEnv :: Read a => String -> a -> IO a
readEnv key def = do
    mVal <- lookupEnv key
    pure $ case mVal of
        Just val -> case readMaybe val of
            Just rval -> rval
            _ -> def
        _ -> def

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
        suites = [CIntSetProps.runTests]
