{-# LANGUAGE ForeignFunctionInterface #-}
module CBTree (
    CBin(..),
    initBtree,
    btFind,
    btInsert,
    btDelete,
    btreeEq,
    btSize,
    btDepth,
    btFromList,
    btToList,
    printBtree,
    freeBtree,

    cstrcmp,
    cstreq,
    cprintstr,
    cKeys,
    freeKeys,
    bTreeFromKeys,
    deleteKeys
) where

import Data.List (genericLength)
import Control.Monad (foldM)
import Foreign
import Foreign.C.Types
import Foreign.C.String

type CmpFn a b = FunPtr (Ptr a -> Ptr b -> IO Int32)
type EqFn a b = FunPtr (Ptr a -> Ptr b -> IO Bool)
type CBinPtr k v = Ptr (CBin k v)

data CAssoc k v = CAssoc (Ptr k) (Ptr v)
    deriving (Eq, Ord, Show)

data CBin k v = CBin
        (CAssoc k v)
        (Ptr (CBin k v))
        (Ptr (CBin k v))
    deriving (Eq, Ord, Show)

instance Storable (CAssoc k v) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr a) * 2
    peek ptr = CAssoc
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1)
        where offs n = alignment (undefined :: CAssoc k v) * n
    poke ptr (CAssoc k v) = do
            pokeByteOff ptr (offs 0) k
            pokeByteOff ptr (offs 1) v
        where offs n = alignment (undefined :: CAssoc k v) * n

instance Storable (CBin k v) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: CAssoc k v) + sizeOf (undefined :: Ptr a) * 2
    peek ptr = CBin
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 3)
        where offs n = alignment (undefined :: CBin k v) * n
    poke ptr (CBin a l r) = do
            pokeByteOff ptr (offs 0) a
            pokeByteOff ptr (offs 2) l
            pokeByteOff ptr (offs 3) r
        where offs n = alignment (undefined :: CBin k v) * n

foreign import ccall "&strcmp" cstrcmp :: CmpFn a b
foreign import ccall "&streq" cstreq :: EqFn a b
foreign import ccall "&printstr" cprintstr :: FunPtr (CString -> IO ())

foreign import ccall "init_btree" initBtree :: Ptr k -> Ptr v -> CBinPtr k v -> CBinPtr k v -> IO (CBinPtr k v)
foreign import ccall "btfind" btFind :: Ptr k -> CmpFn k k -> CBinPtr k v -> IO (CBinPtr k v)
foreign import ccall "btinsert" btInsert :: Ptr k -> CmpFn k k -> Ptr v -> CBinPtr k v -> IO (CBinPtr k v)
foreign import ccall "btdelete" btDelete :: Ptr k -> CmpFn k k -> CBinPtr k v -> IO (CBinPtr k v)
foreign import ccall "btree_eq" btreeEq :: EqFn k k -> EqFn v v -> CBinPtr k v -> CBinPtr k v -> IO Bool
foreign import ccall "btsize" btSize :: CBinPtr k v -> IO Word64
foreign import ccall "btdepth" btDepth :: CBinPtr k v -> IO Word64
foreign import ccall "btfromlist" btFromList :: Ptr (CAssoc k v) -> Word64 -> CmpFn k k -> IO (CBinPtr k v)
foreign import ccall "bttolist" btToList :: CBinPtr k v -> IO (Ptr (CAssoc k v))
foreign import ccall "print_btree" printBtree :: FunPtr (Ptr k -> IO ()) -> CBinPtr k v -> IO ()
foreign import ccall "free_btree" freeBtree :: CBinPtr k v -> IO ()

cKeys :: [String] -> IO [CString]
cKeys = mapM newCAString

freeKeys :: [CString] -> IO ()
freeKeys = mapM_ free

assocArr :: [CString] -> IO (Ptr (CAssoc CChar ()))
assocArr ks = newArray $ fmap (\k -> CAssoc k nullPtr) ks

bTreeFromKeys :: [CString] -> IO (CBinPtr CChar ())
bTreeFromKeys ks = do
    arr <- assocArr ks
    tree <- btFromList arr (genericLength ks) cstrcmp
    free arr
    pure tree

deleteKeys :: [CString] -> CBinPtr CChar () -> IO (CBinPtr CChar ())
deleteKeys ks node = foldM (\prev key -> btDelete key cstrcmp prev) node ks


