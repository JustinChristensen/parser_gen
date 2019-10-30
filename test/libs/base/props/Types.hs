module Types where

import Test.QuickCheck
import Data.Set

newtype ShiftedList a = ShiftedList [a]
    deriving (Show, Eq)

shiftedList :: (Integral a, Arbitrary a) => a -> Gen (ShiftedList a)
shiftedList shift = ShiftedList <$> scale lengthScale (listOf elemGen)
    where
        lengthScaleFactor = 50                 -- make longer lists
        lengthScale = (* lengthScaleFactor)
        elemScaleFactor :: Double
        elemScaleFactor  = 1/10                -- constrain the integrals to a smaller range
        elemScale = ceiling . (* elemScaleFactor) . fromIntegral
        elemGen = fmap (+ shift) (scale elemScale arbitrary)

-- Sets skewed to overlap more often than not
--
-- * Bounds (the range) of the elements are proportional to the size of the set
-- * Bounds for set a shifted by a random number scaled by the size
-- * Bounds for set b shifted by a logarithmic scale relative to the bounds for set a
newtype OverlapSets a = OverlapSets (Set a, Set a)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (OverlapSets a) where
    arbitrary = sized $ \sz -> do
            shift <- arbitraryBoundedIntegral
            ShiftedList s <- shiftedList shift
            ShiftedList t <- shiftedList (shift + offset sz)
            pure $ OverlapSets (fromList s, fromList t)
        where
            offsetBase :: Double
            offsetBase = 1.31                -- shift all elements in a set +- logBase shiftBase size
            offset s = ceiling (logBase offsetBase (fromIntegral s))

newtype EqOverlapSets a = EqOverlapSets (OverlapSets a, Bool)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (EqOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ EqOverlapSets (o, s == t)

newtype DisjointOverlapSets a = DisjointOverlapSets (OverlapSets a, Bool)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (DisjointOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ DisjointOverlapSets (o, s `disjoint` t)

newtype UnionOverlapSets a = UnionOverlapSets (OverlapSets a, Set a)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (UnionOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ UnionOverlapSets (o, s `union` t)

newtype IntersectionOverlapSets a = IntersectionOverlapSets (OverlapSets a, Set a)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (IntersectionOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ IntersectionOverlapSets (o, s `intersection` t)

newtype DifferenceOverlapSets a = DifferenceOverlapSets (OverlapSets a, Set a)
    deriving (Show, Eq)

instance (Bounded a, Integral a, Ord a, Arbitrary a) => Arbitrary (DifferenceOverlapSets a) where
    arbitrary = do
        o@(OverlapSets (s, t)) <- arbitrary
        pure $ DifferenceOverlapSets (o, s `difference` t)

