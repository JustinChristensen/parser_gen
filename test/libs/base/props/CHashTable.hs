{-# LANGUAGE ForeignFunctionInterface #-}
module CHashTable (
    CHashTable(..),

    initHashTable,
    htInsert,
    htContains,
    htLookup,
    htDelete,
    freeHashTable
) where

import Foreign
import Data.Void
import Test.QuickCheck
import Test.QuickCheck.Monadic
import CArray

data CHashTable = CHashTable {
    buckets :: Ptr CArray,
    size :: Ptr Word32,
    used :: Word32,
    entries :: Word32
} deriving (Eq, Ord, Show)

instance Storable CHashTable where
    alignment _ = sizeOf (undefined :: Word32)
    sizeOf _ = sizeOf (undefined :: Ptr Void) * 2 + sizeOf (undefined :: Word32)  * 2
    peek ptr = CHashTable
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 4)
            <*> peekByteOff ptr (offs 5)
        where offs n = alignment (undefined :: CHashTable) * n

    poke ptr (CHashTable b s u e) = do
            pokeByteOff ptr (offs 0) b
            pokeByteOff ptr (offs 2) s
            pokeByteOff ptr (offs 4) u
            pokeByteOff ptr (offs 5) e
        where offs n = alignment (undefined :: CHashTable) * n

foreign import ccall "init_hash_table" initHashTable :: Ptr Word32 -> IO (Ptr CHashTable)
foreign import ccall "htinsert" foo :: Void
foreign import ccall "htcontains" foo :: Void
foreign import ccall "htlookup" foo :: Void
foreign import ccall "htdelete" foo :: Void
foreign import ccall "free_hash_table" freeHashTable :: Ptr CHashTable -> IO ()

