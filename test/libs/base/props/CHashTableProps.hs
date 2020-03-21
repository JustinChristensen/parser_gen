{-# LANGUAGE TemplateHaskell #-}
module CHashTableProps (runTests) where

import Data.List (genericLength)
import CHashTable
import Utils
import Types
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_insert_inserts :: [(BetterASCII, Int32)] -> Property
prop_insert_inserts pairs = monadicIO $ do
    ps <- run $ toHashPairs pairs
    array <- run $ newArray ps
    table <- run $ hashTable (fromIntegral $ sizeOf (undefined :: Int32))
    run $ htFromPairs table (genericLength pairs) array
    allInserted <- run $ containsAll ps table
    run $ whenVerbose $ printHashTable printEntryInt table
    run $ free array
    run $ freeHashTable table
    run $ freeHashPairs ps
    assert allInserted

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

