module Types where

import Prelude hiding (null)
import System.Random (Random)
import Test.QuickCheck
import Data.Set

newtype ShiftedList a = ShiftedList [a]
    deriving (Show, Eq)

shiftedList :: (Integral a, Arbitrary a) => a -> Gen (ShiftedList a)
shiftedList shift = ShiftedList <$> scale lengthScale (listOf elemGen)
    where
        lengthScaleFactor = 15                 -- make longer lists
        lengthScale = (* lengthScaleFactor)
        elemScaleFactor :: Double
        elemScaleFactor  = 1/10                -- constrain the integrals to a smaller range
        elemScale = ceiling . (* elemScaleFactor) . fromIntegral
        elemGen = fmap (+ shift) (scale elemScale arbitrary)

newtype InsertResult a = InsertResult (a, Set a, Set a)
    deriving (Eq)

instance Show a => Show (InsertResult a) where
    show (InsertResult (k, s, t)) =
        "k: " ++ show k ++ "\ns: " ++ show s ++ "\nt: " ++ show t ++ "\n"

instance (Random a, Bounded a, Integral a,
    Ord a, Arbitrary a) => Arbitrary (InsertResult a) where
    arbitrary = do
        shift <- arbitraryBoundedIntegral
        ShiftedList s <- shiftedList shift
        let set = fromList s
        key <- if null set then pure 0
               else choose (findMin set, findMax set)
        pure $ InsertResult (key, set, key `insert` set)

newtype DeleteResult a = DeleteResult (a, Set a, Set a)
    deriving (Eq)

instance Show a => Show (DeleteResult a) where
    show (DeleteResult (k, s, t)) =
        "k: " ++ show k ++ "\ns: " ++ show s ++ "\nt: " ++ show t ++ "\n"

instance (Random a, Bounded a, Integral a,
    Ord a, Arbitrary a) => Arbitrary (DeleteResult a) where
    arbitrary = do
        shift <- arbitraryBoundedIntegral
        ShiftedList s <- shiftedList shift
        let set = fromList s
        key <- if null set then pure 0
               else choose (findMin set, findMax set)
        pure $ DeleteResult (key, set, key `delete` set)

-- Sets skewed to overlap more often than not
--
-- * Bounds (the range) of the elements are proportional to the size of the set
-- * Bounds for set a shifted by a random number scaled by the size
-- * Bounds for set b shifted by a logarithmic scale relative to the bounds for set a
newtype OverlapSets a = OverlapSets (Set a, Set a)
    deriving (Eq)

instance Show a => Show (OverlapSets a) where
    show (OverlapSets (s, t)) =
        "s: " ++ show s ++ "\nt: " ++ show t ++ "\n"

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (OverlapSets a) where
    arbitrary = do
        shift <- arbitraryBoundedIntegral
        ShiftedList s <- shiftedList shift
        ShiftedList t <- shiftedList shift
        pure $ OverlapSets (fromList s, fromList t)

newtype EqOverlapSets a = EqOverlapSets (OverlapSets a, Bool)
    deriving (Eq)

instance Show a => Show (EqOverlapSets a) where
    show (EqOverlapSets (o, x)) =
        show o ++ "= " ++ show x

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (EqOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ EqOverlapSets (o, s == t)

newtype DisjointOverlapSets a = DisjointOverlapSets (OverlapSets a, Bool)
    deriving (Eq)

instance Show a => Show (DisjointOverlapSets a) where
    show (DisjointOverlapSets (o, x)) =
        show o ++ "= " ++ show x

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (DisjointOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ DisjointOverlapSets (o, s `disjoint` t)

newtype UnionOverlapSets a = UnionOverlapSets (OverlapSets a, Set a)
    deriving (Eq)

instance Show a => Show (UnionOverlapSets a) where
    show (UnionOverlapSets (o, x)) =
        show o ++ "u: " ++ show x

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (UnionOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ UnionOverlapSets (o, s `union` t)

newtype IntersectionOverlapSets a = IntersectionOverlapSets (OverlapSets a, Set a)
    deriving (Eq)

instance Show a => Show (IntersectionOverlapSets a) where
    show (IntersectionOverlapSets (o, x)) =
        show o ++ "u: " ++ show x

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (IntersectionOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ IntersectionOverlapSets (o, s `intersection` t)

newtype DifferenceOverlapSets a = DifferenceOverlapSets (OverlapSets a, Set a)
    deriving (Eq)

instance Show a => Show (DifferenceOverlapSets a) where
    show (DifferenceOverlapSets (o, x)) =
        show o ++ "u: " ++ show x

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (DifferenceOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ DifferenceOverlapSets (o, s `difference` t)

