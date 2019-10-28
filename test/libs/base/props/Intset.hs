{-# LANGUAGE ForeignFunctionInterface #-}
module IntSet (
    IntSet(..),
    initIntSet,
    intseteq,
    sunion,
    sintersection,
    sdifference,
    sdisjoint,
    sFromList,
    printIntSet,
    printIntSetTree,
    freeIntSet
) where

import Foreign
import Data.Void
import Control.Applicative ((<$>), (<*>))

data IntSet = IntSet {
    pfix :: Word64,
    mask :: Word64,
    left :: Ptr IntSet,
    right :: Ptr IntSet
} deriving (Eq, Ord, Show)

instance Storable IntSet where
    alignment _ = sizeOf (undefined :: Word64)
    sizeOf _ = sizeOf (undefined :: Word64) * 2 + sizeOf (undefined :: Ptr Void) * 2
    peek ptr = IntSet
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 24
    poke ptr (IntSet p m l r) = do
        pokeByteOff ptr 0 p
        pokeByteOff ptr 8 m
        pokeByteOff ptr 16 l
        pokeByteOff ptr 24 r

foreign import ccall "init_intset" initIntSet :: Word64 -> Word64 -> Ptr IntSet -> Ptr IntSet -> IO (Ptr IntSet)
foreign import ccall "intseteq" intseteq :: Ptr IntSet -> Ptr IntSet -> Bool
foreign import ccall "sunion" sunion :: Ptr IntSet -> Ptr IntSet -> IO (Ptr IntSet)
foreign import ccall "sintersection" sintersection :: Ptr IntSet -> Ptr IntSet -> IO (Ptr IntSet)
foreign import ccall "sdifference" sdifference :: Ptr IntSet -> Ptr IntSet -> IO (Ptr IntSet)
foreign import ccall "sdisjoint" sdisjoint :: Ptr IntSet -> Ptr IntSet -> Bool
foreign import ccall "sfromlist" sFromList :: Ptr Int32 -> Word64 -> IO (Ptr IntSet)
foreign import ccall "print_intset" printIntSet :: Ptr IntSet -> IO ()
foreign import ccall "print_intset_tree" printIntSetTree :: Ptr IntSet -> IO ()
foreign import ccall "free_intset" freeIntSet :: Ptr IntSet -> IO ()
