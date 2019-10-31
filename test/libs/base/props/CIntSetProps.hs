{-# LANGUAGE TemplateHaskell #-}
module CIntSetProps (runTests) where

import Types
import CIntSet
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Set (size)

prop_insert_inserts :: InsertResult Int32 -> Property
prop_insert_inserts (InsertResult (k, s, t)) =
    withCIntSets [s, t] $ \[cs, ct] -> do
        monitor $ label $ if s == t then "Not Inserted" else "Inserted"
        cs' <- run (sinsert k cs)
        equal <- run (intseteq cs' ct)
        freeCIntSets [cs', ct]
        assert equal

prop_delete_deletes :: DeleteResult Int32 -> Property
prop_delete_deletes (DeleteResult (k, s, t)) =
    withCIntSets [s, t] $ \[cs, ct] -> do
        monitor $ label $ if s == t then "Not Deleted" else "Deleted"
        cs' <- run (sdelete k cs)
        equal <- run (intseteq cs' ct)
        freeCIntSets [cs', ct]
        assert equal

prop_eq_sets_are_eq :: EqOverlapSets Int32 -> Property
prop_eq_sets_are_eq (EqOverlapSets (OverlapSets (s, t), equal)) =
    withCIntSets [s, t] $ \csets@[cs, ct] -> do
        monitor $ label $ if equal then "Equal" else "Not Equal"
        res <- run (intseteq cs ct)
        freeCIntSets csets
        assert (res == equal)

prop_disjoint_sets_are_disjoint :: DisjointOverlapSets Int32 -> Property
prop_disjoint_sets_are_disjoint (DisjointOverlapSets (OverlapSets (s, t), equal)) =
    withCIntSets [s, t] $ \csets@[cs, ct] -> do
        monitor $ label $ if equal then "Disjoint" else "Not Disjoint"
        res <- run (sdisjoint cs ct)
        freeCIntSets csets
        assert (res == equal)

prop_sets_union_eq_union :: UnionOverlapSets Int32 -> Property
prop_sets_union_eq_union (UnionOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \csets@[cs, ct, cu] -> do
        monitor $ collect $ size u
        cu' <- run (sunion cs ct)
        equal <- run (intseteq cu cu')
        freeCIntSets (cu' : csets)
        assert equal

prop_sets_intersection_eq_intersection :: IntersectionOverlapSets Int32 -> Property
prop_sets_intersection_eq_intersection (IntersectionOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \csets@[cs, ct, cu] -> do
        monitor $ collect $ size u
        cu' <- run (sintersection cs ct)
        equal <- run (intseteq cu cu')
        freeCIntSets (cu' : csets)
        assert equal

prop_sets_difference_eq_difference :: DifferenceOverlapSets Int32 -> Property
prop_sets_difference_eq_difference (DifferenceOverlapSets (OverlapSets (s, t), u)) =
    withCIntSets [s, t, u] $ \csets@[cs, ct, cu] -> do
        monitor $ collect $ size u
        cu' <- run (sdifference cs ct)
        equal <- run (intseteq cu cu')
        freeCIntSets (cu' : csets)
        assert equal

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests fn = $forAllProperties fn

