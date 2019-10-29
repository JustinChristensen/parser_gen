module Types (
) where

import Test.QuickCheck
import Data.Set
import Foreign

-- Sets skewed to overlap more often than not
newtype OverlapSets a = OverlapSets a {
        getSets :: (Set a, Set a)
    } deriving (Show, Eq)

-- whether the potentially overlapping sets are equivalent
newtype EqOverlapSets a = EqOverlapSets a {
        getSetsEq :: (OverlapSets a, Bool)
    } deriving (Show, Eq)

newtype DisjointOverlapSets a = DisjointOverlapSets a {
        getSetsDisjoint :: (OverlapSets a, Bool)
    } deriving (Show, Eq)

newtype UnionOverlapSets a = UnionOverlapSets a {
        getSetsUnion :: (OverlapSets a, Set a)
    } deriving (Show, Eq)

newtype IntersectionOverlapSets a = IntersectionOverlapSets a {
        getSetsIntersection :: (OverlapSets a, Set a)
    } deriving (Show, Eq)

newtype DifferenceOverlapSets a = DifferenceOverlapSets a {
        getSetsDifference :: (OverlapSets a, Set a)
    } deriving (Show, Eq)

instance Arbitrary a => Arbitrary (OverlapSets a) where

instance Arbitrary a => Arbitrary (EqOverlapSets a) where

instance Arbitrary a => Arbitrary (UnionOverlapSets a) where

instance Arbitrary a => Arbitrary (IntersectionOverlapSets a) where

instance Arbitrary a => Arbitrary (DifferenceOverlapSets a) where

instance Arbitrary a => Arbitrary (DisjointOverlapSets a) where
