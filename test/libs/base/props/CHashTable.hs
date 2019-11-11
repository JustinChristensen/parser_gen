{-# LANGUAGE ForeignFunctionInterface #-}
module CHashTable (
    CHashTable(..),
    FromEntry(..),
    initHashTable,
    htInsertI,
    htContains,
    htLookupI,
    htDelete,
    toEntryList,
    fromEntryList,
    htSize,
    htEntries,
    htUsed,
    printEntryInt,
    printHashTable,
    printHashEntries,
    freeHashTable,

    fromHashEntry,
    toHashEntry,
    toHashEntries,
    freeHashEntries,
    containsAll
) where

import Test.QuickCheck
import Foreign
import Foreign.C.String
import CArray

type Entry = Ptr
type Bucket a = CArray (CHashEntry a)
type CHashTablePtr a = Ptr (CHashTable a)

data CHashEntry a = CHashEntry CString (Entry a)
    deriving (Eq, Ord, Show)

data CHashTable a = CHashTable {
    buckets :: Ptr (Bucket a),
    size :: Ptr Word32,
    used :: Word32,
    entries :: Word32
} deriving (Eq, Ord, Show)

instance Storable (CHashEntry a) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: CString) + sizeOf (undefined :: Ptr a)
    peek ptr = CHashEntry
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1)
        where offs n = alignment (undefined :: CHashEntry a) * n
    poke ptr (CHashEntry k v) = do
            pokeByteOff ptr (offs 0) k
            pokeByteOff ptr (offs 1) v
        where offs n = alignment (undefined :: CHashEntry a) * n

instance Storable (CHashTable a) where
    alignment _ = sizeOf (undefined :: Word32)
    sizeOf _ = sizeOf (undefined :: Ptr a) * 2 + sizeOf (undefined :: Word32)  * 2
    peek ptr = CHashTable
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 4)
            <*> peekByteOff ptr (offs 5)
        where offs n = alignment (undefined :: CHashTable a) * n
    poke ptr (CHashTable b s u e) = do
            pokeByteOff ptr (offs 0) b
            pokeByteOff ptr (offs 2) s
            pokeByteOff ptr (offs 4) u
            pokeByteOff ptr (offs 5) e
        where offs n = alignment (undefined :: CHashTable a) * n

class FromEntry a where
    asPtr :: a -> Ptr b
    asInt32 :: a -> Int32

instance FromEntry (Ptr a) where
    asPtr = castPtr
    asInt32 p = fromIntegral i
        where IntPtr i = ptrToIntPtr p

instance FromEntry Int32 where
    asPtr i = intPtrToPtr $ IntPtr (fromIntegral i)
    asInt32 = id

foreign import ccall "&print_entry_int" printEntryInt :: FunPtr (Entry a -> IO ())

foreign import ccall "init_hash_table" initHashTable :: Ptr Word32 -> IO (CHashTablePtr a)
foreign import ccall "htinsert_i" htInsertI :: CString -> Int32 -> CHashTablePtr a -> IO (CHashTablePtr a)
foreign import ccall "htcontains" htContains :: CString -> CHashTablePtr a -> IO Bool
foreign import ccall "htlookup_i" htLookupI :: Ptr Int32 -> CString -> CHashTablePtr a -> IO Bool
foreign import ccall "htdelete" htDelete :: CString -> CHashTablePtr a -> IO Bool
foreign import ccall "from_entry_list" fromEntryList :: Ptr (CHashEntry a) -> Word64 -> IO (CHashTablePtr a)
foreign import ccall "to_entry_list" toEntryList :: CHashTablePtr a -> Ptr (CHashEntry a)
foreign import ccall "htsize" htSize :: IO Word32
foreign import ccall "htentries" htEntries :: IO Word32
foreign import ccall "htused" htUsed :: IO Word32
foreign import ccall "print_hash_table" printHashTable :: FunPtr (Entry a -> IO ()) -> CHashTablePtr a -> IO ()
foreign import ccall "print_hash_entries" printHashEntries :: FunPtr (Entry a -> IO ()) -> CHashTablePtr a -> IO ()
foreign import ccall "free_hash_table" freeHashTable :: CHashTablePtr a -> IO ()

toHashEntry :: (PrintableString, Int32) -> IO (CHashEntry ())
toHashEntry (PrintableString k, v) = do
    key <- newCString k
    pure $ CHashEntry key (asPtr v)

fromHashEntry :: CHashEntry () -> IO (String, Int32)
fromHashEntry (CHashEntry k v) = do
    str <- peekCString k
    pure $ (str, asInt32 v)

toHashEntries :: [(PrintableString, Int32)] -> IO [CHashEntry ()]
toHashEntries pairs = mapM toHashEntry pairs

freeHashEntries :: [CHashEntry ()] -> IO ()
freeHashEntries = mapM_ freeHashEntry
    where freeHashEntry (CHashEntry k _) = free k

containsAll :: [CHashEntry ()] -> Ptr (CHashTable ()) -> IO Bool
containsAll es t = and <$> mapM containsEntry es
    where containsEntry (CHashEntry k _) = htContains k t

