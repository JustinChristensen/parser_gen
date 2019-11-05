{-# LANGUAGE ForeignFunctionInterface #-}
module CArray (
    CArray(..)
) where

import Foreign

data Growth = Exponential | Linear | Frozen
    deriving (Eq, Ord, Show, Enum)

data CArray a = CArray
        (Ptr a)   -- buf
        Growth  -- growth
        Float   -- factor
        Int32   -- i
        Word32  -- init_size
        Word32  -- size
        Word32  -- elem_size
    deriving (Eq, Ord, Show)

instance Storable (CArray a) where
    alignment _ = sizeOf (undefined :: Word32)
    sizeOf _
        = sizeOf (undefined :: Ptr a)
        + sizeOf (undefined :: Word32) * 4
        + sizeOf (undefined :: Float)
        + sizeOf (undefined :: Int32)
    peek ptr = CArray
            <$> peekByteOff ptr (offs 0)
            <*> fmap toEnum (peekByteOff ptr (offs 2))
            <*> peekByteOff ptr (offs 3)
            <*> peekByteOff ptr (offs 4)
            <*> peekByteOff ptr (offs 5)
            <*> peekByteOff ptr (offs 6)
            <*> peekByteOff ptr (offs 7)
        where offs n = alignment (undefined :: CArray a) * n
    poke ptr (CArray b g f ind is s es) = do
            pokeByteOff ptr (offs 0) b
            pokeByteOff ptr (offs 2) (fromEnum g)
            pokeByteOff ptr (offs 3) f
            pokeByteOff ptr (offs 4) ind
            pokeByteOff ptr (offs 5) is
            pokeByteOff ptr (offs 6) s
            pokeByteOff ptr (offs 7) es
        where offs n = alignment (undefined :: CArray a) * n
