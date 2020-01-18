{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language KindSignatures #-}
{-# language PatternGuards #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language CPP #-}
module X86.Generator.Syntax.Sizes where

import Numeric
import Data.List
import Data.Bits
import Data.Int
import Data.Word
import Control.Monad
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

class HasSize a where
  size :: a -> Size
  sizeValueOf :: a -> Int
  sizeValueOf a = sizeValue $ size a

class WithTypedSize (s :: Size) where
  getSizeOf :: TypedSize s

-- | The size of a register (in bits)
data Size = Size1B | Size8B | Size16B | Size32B | Size64B | Size128B
  deriving (Eq, Ord)

-- | Singleton type for size
data TypedSize (s :: Size) where
  TypedSize1   :: TypedSize Size1B
  TypedSize8   :: TypedSize Size8B
  TypedSize16  :: TypedSize Size16B
  TypedSize32  :: TypedSize Size32B
  TypedSize64  :: TypedSize Size64B
  TypedSize128 :: TypedSize Size128B

instance Show Size where
  show = \case
    Size1B   -> "bit"
    Size8B   -> "byte"
    Size16B  -> "word"
    Size32B  -> "dword"
    Size64B  -> "qword"
    Size128B -> "oword"

instance HasSize Word8  where size _ = Size8B
instance HasSize Word16 where size _ = Size16B
instance HasSize Word32 where size _ = Size32B
instance HasSize Word64 where size _ = Size64B
instance HasSize Int8   where size _ = Size8B
instance HasSize Int16  where size _ = Size16B
instance HasSize Int32  where size _ = Size32B
instance HasSize Int64  where size _ = Size64B

instance HasSize (TypedSize s) where
  size = \case
    TypedSize1   -> Size1B
    TypedSize8   -> Size8B
    TypedSize16  -> Size16B
    TypedSize32  -> Size32B
    TypedSize64  -> Size64B
    TypedSize128 -> Size128B

instance WithTypedSize Size1B   where getSizeOf = TypedSize1
instance WithTypedSize Size8B   where getSizeOf = TypedSize8
instance WithTypedSize Size16B  where getSizeOf = TypedSize16
instance WithTypedSize Size32B  where getSizeOf = TypedSize32
instance WithTypedSize Size64B  where getSizeOf = TypedSize64
instance WithTypedSize Size128B where getSizeOf = TypedSize128

sizeValue = \case
  Size8B   -> 1
  Size16B  -> 2
  Size32B  -> 4
  Size64B  -> 8
  Size128B -> 16

ofSize  1 = Size8B
ofSize  2 = Size16B
ofSize  4 = Size32B
ofSize  8 = Size64B
ofSize 16 = Size128B