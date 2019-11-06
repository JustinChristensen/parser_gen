{-# LANGUAGE TemplateHaskell #-}
module CHashTableProps (runTests) where

import Data.List (genericLength)
import CHashTable
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_insert_inserts :: [(PrintableString, Int32)] -> Property
prop_insert_inserts pairs = monadicIO $ do
    es <- run $ toHashEntries pairs
    array <- run $ newArray es
    table <- run $ fromEntryList array (genericLength pairs)
    allInserted <- run $ containsAll es table
    run $ printHashTable printEntryInt table
    run $ free array
    run $ freeHashTable table
    run $ freeHashEntries es
    assert allInserted

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

