{-# LANGUAGE ForeignFunctionInterface #-}
module CIntSet (
    CIntSet(..),

    initIntSet,
    intseteq,
    sunion,
    sintersection,
    sdifference,
    sdisjoint,
    sFromList,
    printIntSet,
    printIntSetTree,
    freeIntSet,

    fromList,
    fromSet,
    toCIntSets,
    freeCIntSets,
    withCIntSets
) where

import Foreign
import Data.Void
import Data.Set (Set(..), toList, size)
import Control.Applicative ((<$>), (<*>))
import Data.List (genericLength)
import Test.QuickCheck
import Test.QuickCheck.Monadic

data CIntSet = CIntSet {
    pfix :: Word64,
    mask :: Word64,
    left :: Ptr CIntSet,
    right :: Ptr CIntSet
} deriving (Eq, Ord, Show)

instance Storable CIntSet where
    alignment _ = sizeOf (undefined :: Word64)
    sizeOf _ = sizeOf (undefined :: Word64) * 2 + sizeOf (undefined :: Ptr Void) * 2
    peek ptr = CIntSet
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 3)
        where offs n = alignment (undefined :: CIntSet) * n

    poke ptr (CIntSet p m l r) = do
            pokeByteOff ptr (offs 0) p
            pokeByteOff ptr (offs 1) m
            pokeByteOff ptr (offs 2) l
            pokeByteOff ptr (offs 3) r
        where offs n = alignment (undefined :: CIntSet) * n

foreign import ccall "init_intset" initIntSet :: Word64 -> Word64 -> Ptr CIntSet -> Ptr CIntSet -> IO (Ptr CIntSet)
foreign import ccall "intseteq" intseteq :: Ptr CIntSet -> Ptr CIntSet -> IO Bool
foreign import ccall "sunion" sunion :: Ptr CIntSet -> Ptr CIntSet -> IO (Ptr CIntSet)
foreign import ccall "sintersection" sintersection :: Ptr CIntSet -> Ptr CIntSet -> IO (Ptr CIntSet)
foreign import ccall "sdifference" sdifference :: Ptr CIntSet -> Ptr CIntSet -> IO (Ptr CIntSet)
foreign import ccall "sdisjoint" sdisjoint :: Ptr CIntSet -> Ptr CIntSet -> Bool
foreign import ccall "sfromlist" sFromList :: Ptr Int32 -> Word64 -> IO (Ptr CIntSet)
foreign import ccall "print_intset" printIntSet :: Ptr CIntSet -> IO ()
foreign import ccall "print_intset_tree" printIntSetTree :: Ptr CIntSet -> IO ()
foreign import ccall "free_intset" freeIntSet :: Ptr CIntSet -> IO ()

fromList :: [Int32] -> IO (Ptr CIntSet)
fromList xs = do
    arr <- newArray xs
    sFromList arr (genericLength xs)

fromSet :: Set Int32 -> IO (Ptr CIntSet)
fromSet xs = do
    arr <- newArray (toList xs)
    sFromList arr (size xs)

toCIntSets :: [Set Int32] -> PropertyM IO [Ptr CIntSet]
toCIntSets sets = run (mapM fromSet sets)

freeCIntSets :: [Ptr CIntSet] -> PropertyM IO ()
freeCIntSets sets = run (mapM_ freeIntSet sets)

withCIntSets :: [Set Int32] -> ([Ptr CIntSet] -> PropertyM IO ()) -> Property
withCIntSets sets test = monadicIO $ do
    csets <- toCIntSets sets
    test
    freeCIntSets csets


