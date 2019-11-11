{-# LANGUAGE TemplateHaskell #-}
module CBTreeProps (runTests) where

import Data.List (genericLength)
import CBTree
import qualified Data.Map as M
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_delete_deletes :: SubMapOf PrintableString () -> Property
prop_delete_deletes (SubMapOf (s, t)) = monadicIO $ do
        monitor $ collect (M.size toDelete)
        t1 <- run $ bTreeFromMap s
        t2 <- run $ bTreeFromMap t
    where
        toDelete = s `difference` t

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

