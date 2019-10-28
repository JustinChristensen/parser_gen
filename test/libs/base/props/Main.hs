module Main where

import IntSet
import Foreign
import System.IO (hFlush, stdout)
import Data.List (genericLength)

main :: IO ()
main = do
        withArray xs $ \ints -> do
            is <- sFromList ints (genericLength xs)
            printIntSet is >> hFlush stdout >> putStrLn ""
            printIntSetTree is
            freeIntSet is
    where xs = [-600, 0, 1, 1, 400, 900, 200000]
