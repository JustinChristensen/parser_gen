{-# LANGUAGE TemplateHaskell #-}
module CBTreeProps (runTests) where

import CBTree
import Types
import Control.Monad (unless)
import Data.List (sort)
import Foreign
import Foreign.C.String
import Test.QuickCheck
import Test.QuickCheck.Monadic

unwrapKeys :: [BetterASCII] -> [String]
unwrapKeys = fmap getBetterASCII

-- prop_delete_deletes :: SubMapOf BetterASCII () -> Property
-- prop_delete_deletes (SubMapOf (s, t)) = monadicIO $ do
--         dk <- run keysToDelete
--         monitor $ collect (length dk)
--         -- monitor $ counterexample ("dk: " ++ show diff)
--         [sk, tk] <- run $ mapM cKeys [unwrapKeys s, unwrapKeys t]
--         [bs, bt] <- run $ mapM bTreeFromKeys [sk, tk]
--         bs' <- run $ deleteKeys dk bs
--         equal <- run $ btreeEq cstreq nullFunPtr bt bs'
--         run $ unless equal $ do
--             printBtree cprintstr bt
--             printBtree cprintstr bs'
--         run $ mapM_ freeKeys [sk, tk, dk]
--         run $ mapM_ freeBtree [bs', bt]
--         assert equal
--     where
--         diff = unwrapKeys $ s `M.difference` t
--         keysToDelete = cKeys diff

prop_produces_sorted_keys :: UniqList BetterASCII -> Property
prop_produces_sorted_keys (UniqList keys) = monadicIO $ do
        let strKeys = unwrapKeys keys
        strCKeys <- run $ cKeys strKeys
        tree <- run $ bTreeFromKeys strCKeys
        run $ btInvariants tree True cstrcmp
        treeCKeys <- run $ btKeys tree
        treeSize <- run $ btSize tree
        treeDepth <- run $ btDepth tree
        treeKeys <- run $ do
            arr <- peekArray (fromIntegral treeSize) treeCKeys
            mapM peekCAString arr
        monitor $ label ("size: " ++ show treeSize ++ ", height: " ++ show treeDepth)
        let equal = sort strKeys == treeKeys
        run $ free treeCKeys
        run $ freeKeys strCKeys
        run $ freeBtree tree
        assert equal

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

