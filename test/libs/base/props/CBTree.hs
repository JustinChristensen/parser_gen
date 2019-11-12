{-# LANGUAGE ForeignFunctionInterface #-}
module CBTree (
    CbtNode(..),
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
type CbtNodePtr k v = Ptr (CbtNode k v)

data CAssoc k v = CAssoc (Ptr k) (Ptr v)
    deriving (Eq, Ord, Show)

data CbtNode k v = CbtNode
        (CAssoc k v)
        (Ptr (CbtNode k v))
        (Ptr (CbtNode k v))
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

instance Storable (CbtNode k v) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: CAssoc k v) + sizeOf (undefined :: Ptr a) * 2
    peek ptr = CbtNode
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 3)
        where offs n = alignment (undefined :: CbtNode k v) * n
    poke ptr (CbtNode a l r) = do
            pokeByteOff ptr (offs 0) a
            pokeByteOff ptr (offs 2) l
            pokeByteOff ptr (offs 3) r
        where offs n = alignment (undefined :: CbtNode k v) * n

foreign import ccall "&strcmp" cstrcmp :: CmpFn a b
foreign import ccall "&streq" cstreq :: EqFn a b
foreign import ccall "&printstr" cprintstr :: FunPtr (CString -> IO ())

foreign import ccall "init_btree" initBtree :: Ptr k -> Ptr v -> CbtNodePtr k v -> CbtNodePtr k v -> IO (CbtNodePtr k v)
foreign import ccall "btfind" btFind :: Ptr k -> CmpFn k k -> CbtNodePtr k v -> IO (CbtNodePtr k v)
foreign import ccall "btinsert" btInsert :: Ptr k -> CmpFn k k -> Ptr v -> CbtNodePtr k v -> IO (CbtNodePtr k v)
foreign import ccall "btdelete" btDelete :: Ptr k -> CmpFn k k -> CbtNodePtr k v -> IO (CbtNodePtr k v)
foreign import ccall "btree_eq" btreeEq :: EqFn k k -> EqFn v v -> CbtNodePtr k v -> CbtNodePtr k v -> IO Bool
foreign import ccall "btsize" btSize :: CbtNodePtr k v -> IO Word64
foreign import ccall "btdepth" btDepth :: CbtNodePtr k v -> IO Word64
foreign import ccall "btfromlist" btFromList :: Ptr (CAssoc k v) -> Word64 -> CmpFn k k -> IO (CbtNodePtr k v)
foreign import ccall "bttolist" btToList :: CbtNodePtr k v -> IO (Ptr (CAssoc k v))
foreign import ccall "print_btree" printBtree :: FunPtr (Ptr k -> IO ()) -> CbtNodePtr k v -> IO ()
foreign import ccall "free_btree" freeBtree :: CbtNodePtr k v -> IO ()

cKeys :: [String] -> IO [CString]
cKeys = mapM newCAString

freeKeys :: [CString] -> IO ()
freeKeys = mapM_ free

assocArr :: [CString] -> IO (Ptr (CAssoc CChar ()))
assocArr ks = newArray $ fmap (\k -> CAssoc k nullPtr) ks

bTreeFromKeys :: [CString] -> IO (CbtNodePtr CChar ())
bTreeFromKeys ks = do
    arr <- assocArr ks
    tree <- btFromList arr (genericLength ks) cstrcmp
    free arr
    pure tree

deleteKeys :: [CString] -> CbtNodePtr CChar () -> IO (CbtNodePtr CChar ())
deleteKeys ks node = foldM (\prev key -> btDelete key cstrcmp prev) node ks


