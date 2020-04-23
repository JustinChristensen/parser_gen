{-# LANGUAGE ForeignFunctionInterface #-}
module CRBTree (
    CRbNode(..),
    rbFind,
    rbInsert,
    rbDelete,
    rbreeEq,
    rbSize,
    rbDepth,
    rbFromList,
    rbToList,
    rbKeys,
    rbInvariants,
    printRbTree,
    freeRbTree,

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
type CRbNodePtr k v = Ptr (CRbNode k v)

data CRbAssoc k v = CRbAssoc (Ptr k) (Ptr v)
    deriving (Eq, Ord, Show)

data CRbNode k v = CRbNode
        (CRbAssoc k v)
        Bool
        (Ptr (CRbNode k v))
        (Ptr (CRbNode k v))
    deriving (Eq, Ord, Show)

instance Storable (CRbAssoc k v) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr a) * 2
    peek ptr = CRbAssoc
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 1)
        where offs n = alignment (undefined :: CRbAssoc k v) * n
    poke ptr (CRbAssoc k v) = do
            pokeByteOff ptr (offs 0) k
            pokeByteOff ptr (offs 1) v
        where offs n = alignment (undefined :: CRbAssoc k v) * n

instance Storable (CRbNode k v) where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: CRbAssoc k v) + sizeOf (undefined :: Ptr a) * 2
    peek ptr = CRbNode
            <$> peekByteOff ptr (offs 0)
            <*> peekByteOff ptr (offs 2)
            <*> peekByteOff ptr (offs 3)
            <*> peekByteOff ptr (offs 4)
        where offs n = alignment (undefined :: CRbNode k v) * n
    poke ptr (CRbNode a c l r) = do
            pokeByteOff ptr (offs 0) a
            pokeByteOff ptr (offs 2) c
            pokeByteOff ptr (offs 3) l
            pokeByteOff ptr (offs 4) r
        where offs n = alignment (undefined :: CRbNode k v) * n

foreign import ccall "&strcmp" cstrcmp :: CmpFn a b
foreign import ccall "&streq" cstreq :: EqFn a b
foreign import ccall "&printstr" cprintstr :: FunPtr (CString -> IO ())

foreign import ccall "rbfind" rbFind :: Ptr k -> CmpFn k k -> CRbNodePtr k v -> IO (CRbNodePtr k v)
foreign import ccall "rbinsert" rbInsert :: Ptr k -> CmpFn k k -> Ptr v -> CRbNodePtr k v -> IO (CRbNodePtr k v)
foreign import ccall "rbdelete" rbDelete :: Ptr k -> CmpFn k k -> CRbNodePtr k v -> IO (CRbNodePtr k v)
foreign import ccall "rbree_eq" rbreeEq :: EqFn k k -> EqFn v v -> CRbNodePtr k v -> CRbNodePtr k v -> IO Bool
foreign import ccall "rbsize" rbSize :: CRbNodePtr k v -> IO Word64
foreign import ccall "rbdepth" rbDepth :: CRbNodePtr k v -> IO Word64
foreign import ccall "rbfromlist" rbFromList :: Ptr (CRbAssoc k v) -> Word64 -> CmpFn k k -> IO (CRbNodePtr k v)
foreign import ccall "rbtolist" rbToList :: CRbNodePtr k v -> IO (Ptr (CRbAssoc k v))
foreign import ccall "rbkeys" rbKeys :: CRbNodePtr k v -> IO (Ptr (Ptr k))
foreign import ccall "rbinvariants" rbInvariants :: CRbNodePtr k v -> Bool -> CmpFn k k -> IO ()
foreign import ccall "print_rbtree" printRbTree :: FunPtr (Ptr k -> IO ()) -> CRbNodePtr k v -> IO ()
foreign import ccall "free_rbtree" freeRbTree :: CRbNodePtr k v -> IO ()

cKeys :: [String] -> IO [CString]
cKeys = mapM newCAString

freeKeys :: [CString] -> IO ()
freeKeys = mapM_ free

assocArr :: [CString] -> IO (Ptr (CRbAssoc CChar ()))
assocArr ks = newArray $ fmap (\k -> CRbAssoc k nullPtr) ks

bTreeFromKeys :: [CString] -> IO (CRbNodePtr CChar ())
bTreeFromKeys ks = do
    arr <- assocArr ks
    tree <- rbFromList arr (genericLength ks) cstrcmp
    free arr
    pure tree

deleteKeys :: [CString] -> CRbNodePtr CChar () -> IO (CRbNodePtr CChar ())
deleteKeys ks node = foldM (\prev key -> rbDelete key cstrcmp prev) node ks


