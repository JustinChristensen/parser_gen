{-# LANGUAGE TemplateHaskell #-}
module CIntSetProps (runTests) where

import Types
import CIntSet
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_eq_sets_are_eq :: EqOverlapSets Int32 -> Property
prop_eq_sets_are_eq (EqOverlapSets (OverlapSets (s, t), equal)) =
    withCIntSets [s, t] $ \[cs, ct] -> do
        res <- run (intseteq cs ct)
        assert (res == equal)

prop_disjoint_sets_are_disjoint :: DisjointOverlapSets Int32 -> Property
prop_disjoint_sets_are_disjoint (DisjointOverlapSets (OverlapSets (s, t), equal)) =
    withCIntSets [s, t] $ \[cs, ct] -> do
        res <- run (intseteq cs ct)
        assert (res == equal)

prop_sets_union_eq_union :: UnionOverlapSets Int32 -> Property
prop_sets_union_eq_union (UnionOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \[cs, ct, cu] -> do
        cu' <- run (sunion cs ct)
        equal <- run (intseteq cu cu')
        run (freeIntSet cu')
        assert equal

prop_sets_intersection_eq_intersection :: IntersectionOverlapSets Int32 -> Property
prop_sets_intersection_eq_intersection (IntersectionOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \[cs, ct, cu] -> do
        cu' <- run (sintersection cs ct)
        equal <- run (intseteq cu cu')
        run (freeIntSet cu')
        assert equal

prop_sets_difference_eq_difference :: DifferenceOverlapSets Int32 -> Property
prop_sets_difference_eq_difference (DifferenceOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \[cs, ct, cu] -> do
        cu' <- run (sdifference cs ct)
        equal <- run (intseteq cu cu')
        run (freeIntSet cu')
        assert equal

return []
runTests :: IO Bool
-- runTests = $verboseCheckAll
runTests = $quickCheckAll

