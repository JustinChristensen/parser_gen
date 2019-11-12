{-# LANGUAGE TemplateHaskell #-}
module CBTreeProps (runTests) where

import CBTree
import Types
import qualified Data.Map as M
import Control.Monad (unless)
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_delete_deletes :: SubMapOf BetterASCII () -> Property
prop_delete_deletes (SubMapOf (s, t)) = monadicIO $ do
        dk <- run keysToDelete
        monitor $ collect (length dk)
        -- monitor $ counterexample ("dk: " ++ show diff)
        [sk, tk] <- run $ mapM cKeys [unwrapKeys s, unwrapKeys t]
        [bs, bt] <- run $ mapM bTreeFromKeys [sk, tk]
        bs' <- run $ deleteKeys dk bs
        equal <- run $ btreeEq cstreq nullFunPtr bt bs'
        run $ unless equal $ do
            printBtree cprintstr bt
            printBtree cprintstr bs'
        run $ mapM_ freeKeys [sk, tk, dk]
        run $ mapM_ freeBtree [bs', bt]
        assert equal
    where
        diff = unwrapKeys $ s `M.difference` t
        unwrapKeys m = getBetterASCII <$> M.keys m
        keysToDelete = cKeys diff

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

