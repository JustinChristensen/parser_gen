{-# LANGUAGE TemplateHaskell #-}
module IntSetProps (runTests) where

import IntSet
import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.List (genericLength)

fromL :: [Int32] -> IO (Ptr IntSet)
fromL xs = do
    arr <- newArray xs
    sFromList arr (genericLength xs)

prop_eqSetsAreEq :: [Int32] -> Property
prop_eqSetsAreEq xs = monadicIO $ do
    (s, t) <- run $ do
        s <- fromL xs
        t <- fromL xs
        pure (s, t)

    res <- run $ intseteq s t

    run $ do
        freeIntSet s
        freeIntSet t

    assert res

prop_unEqSetsAreUnEq :: [Int32] -> [Int32] -> Property
prop_unEqSetsAreUnEq xs ys = monadicIO $ do
    (s, t, u, v) <- run $ do
        s <- fromL xs
        t <- fromL ys
        u <- sintersection s t
        v <- sdifference s t
        pure (s, t, u, v)

    run $ do
        printIntSetTree s
        printIntSetTree u

    res <- run $ intseteq s u

    run $ do
        freeIntSet s
        freeIntSet t
        freeIntSet u
        freeIntSet v

    assert res

return []
runTests :: IO Bool
runTests = $verboseCheckAll
