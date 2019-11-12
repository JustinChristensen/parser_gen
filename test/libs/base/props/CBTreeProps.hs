{-# LANGUAGE TemplateHaskell #-}
module CBTreeProps (runTests) where

import CBTree
import Types
import qualified Data.Map as M
import Control.Monad (unless)
import Foreign
import Foreign.C.String
import Test.QuickCheck
import Test.QuickCheck.Monadic

unwrapKeys :: M.Map BetterASCII () -> [String]
unwrapKeys m = getBetterASCII <$> M.keys m

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
        keysToDelete = cKeys diff

prop_produces_sorted_keys :: M.Map BetterASCII () -> Property
prop_produces_sorted_keys m = monadicIO $ do
        let mKeys = unwrapKeys m
        mCKeys <- run $ cKeys mKeys
        tree <- run $ bTreeFromKeys mCKeys
        treeCKeys <- run $ btKeys tree
        treeSize <- run $ btSize tree
        treeKeys <- run $ do
            arr <- peekArray (fromIntegral treeSize) treeCKeys
            mapM peekCAString arr
        monitor $ collect (length treeKeys)
        let equal = mKeys == treeKeys
        run $ free treeCKeys
        run $ freeKeys mCKeys
        run $ freeBtree tree
        assert equal

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

