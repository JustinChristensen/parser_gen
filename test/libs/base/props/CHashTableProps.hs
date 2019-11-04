{-# LANGUAGE TemplateHaskell #-}
module CHashTableProps (runTests) where

import CHashTable
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

