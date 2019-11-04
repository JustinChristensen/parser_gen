{-# LANGUAGE ForeignFunctionInterface #-}
module CArray (
    CArray(..)
) where

import Foreign
import Data.Void
import Test.QuickCheck
import Test.QuickCheck.Monadic

data CArray = CArray {
    buf :: Ptr Void,
    growth :: Word32,
    factor :: Float,
    i :: Int32,
    initSize :: Word32,
    size :: Word32,
    elemSize :: Word32
} deriving (Eq, Ord, Show)

instance Storable CArray where
    alignment _ = sizeOf (undefined :: Word32)
    sizeOf _
        = sizeOf (undefined :: Ptr Void)
        + sizeOf (undefined :: Word32) * 4
        + sizeOf (undefined :: Float)
        + sizeOf (undefined :: Int32)
    peek ptr = CArray
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 3)
            <*> peekByteOff ptr (offs 4)
            <*> peekByteOff ptr (offs 5)
            <*> peekByteOff ptr (offs 6)
            <*> peekByteOff ptr (offs 7)
        where offs n = alignment (undefined :: CArray) * n

    poke ptr (CArray b g f i is s es) = do
            pokeByteOff ptr (offs 0) b
            pokeByteOff ptr (offs 2) g
            pokeByteOff ptr (offs 3) f
            pokeByteOff ptr (offs 4) i
            pokeByteOff ptr (offs 5) is
            pokeByteOff ptr (offs 6) s
            pokeByteOff ptr (offs 7) es
        where offs n = alignment (undefined :: CArray) * n
