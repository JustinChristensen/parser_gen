{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CHashTable (
    CHashTable(..),
    hashTable,
    freeHashTable,
    htInsertI,
    htContains,
    htLookup,
    htDelete,
    htPairs,
    htFromPairs,
    htEntries,
    printEntryInt,
    printHashTable,
    printHashEntries,

    fromHashPair,
    toHashPair,
    toHashPairs,
    freeHashPairs,
    containsAll
) where

import Types
import Foreign
import Foreign.C.String

newtype CHashPair a = CHashPair (CString, a)
type CHashTablePtr a = Ptr (CHashTable a)

data CHashTable a = CHashTable {
    valsize :: Word64,
    buckets :: Ptr a,
    size :: Ptr Word32,
    used :: Word32,
    entries :: Word32
} deriving (Eq, Ord, Show)

instance Storable a => Storable (CHashPair a) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr a) + sizeOf (undefined :: a)
    peek ptr = CHashPair <$> ((,)
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1))
        where offs n = alignment (undefined :: CHashPair a) * n
    poke ptr (CHashPair (k, v)) = do
            pokeByteOff ptr (offs 0) k
            pokeByteOff ptr (offs 1) v
        where offs n = alignment (undefined :: CHashPair a) * n

instance Storable (CHashTable a) where
    alignment _ = sizeOf (undefined :: Word32)
    sizeOf _ = sizeOf (undefined :: Word64) + sizeOf (undefined :: Ptr a) * 2 + sizeOf (undefined :: Word32)  * 2
    peek ptr = CHashTable
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1)
            <*> peekByteOff ptr (offs 3)
            <*> peekByteOff ptr (offs 5)
            <*> peekByteOff ptr (offs 6)
        where offs n = alignment (undefined :: CHashTable a) * n
    poke ptr (CHashTable v b s u e) = do
            pokeByteOff ptr (offs 0) v
            pokeByteOff ptr (offs 1) b
            pokeByteOff ptr (offs 3) s
            pokeByteOff ptr (offs 5) u
            pokeByteOff ptr (offs 6) e
        where offs n = alignment (undefined :: CHashTable a) * n

foreign import ccall "&print_entry_int" printEntryInt :: FunPtr (Ptr a -> IO ())

foreign import ccall "hash_table" hashTable :: Word64 -> IO (CHashTablePtr a)
foreign import ccall "free_hash_table" freeHashTable :: CHashTablePtr a -> IO ()
foreign import ccall "htinsert_i" htInsertI :: CString -> Int32 -> CHashTablePtr a -> IO ()
foreign import ccall "htcontains" htContains :: CString -> CHashTablePtr a -> IO Bool
foreign import ccall "htlookup" htLookup :: CString -> CHashTablePtr a -> Ptr a
foreign import ccall "htdelete" htDelete :: CString -> CHashTablePtr a -> IO Bool
foreign import ccall "htpairs" htPairs :: CHashTablePtr a -> IO (Ptr (CHashPair a))
foreign import ccall "htfrompairs" htFromPairs :: CHashTablePtr a -> Word32 -> Ptr (CHashPair a) -> IO ()
foreign import ccall "htentries" htEntries :: IO Word32
foreign import ccall "print_hash_table" printHashTable :: FunPtr (Ptr a -> IO ()) -> CHashTablePtr a -> IO ()
foreign import ccall "print_hash_entries" printHashEntries :: FunPtr (Ptr a -> IO ()) -> CHashTablePtr a -> IO ()

toHashPair :: (BetterASCII, a) -> IO (CHashPair a)
toHashPair (BetterASCII k, v) = do
    key <- newCAString k
    pure $ CHashPair (key, v)

fromHashPair :: CHashPair a -> IO (String, a)
fromHashPair (CHashPair (k, v)) = do
    str <- peekCString k
    pure $ (str, v)

toHashPairs :: [(BetterASCII, a)] -> IO [CHashPair a]
toHashPairs pairs = mapM toHashPair pairs

freeHashPairs :: [CHashPair a] -> IO ()
freeHashPairs = mapM_ freeHashPair
    where freeHashPair (CHashPair (k, _)) = free k

containsAll :: [CHashPair a] -> Ptr (CHashTable a) -> IO Bool
containsAll ps t = and <$> mapM containsEntry ps
    where containsEntry (CHashPair (k, _)) = htContains k t

