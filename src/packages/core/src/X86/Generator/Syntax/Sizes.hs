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

-- | The size of a register (in bits)
data Size = S1 | S8 | S16 | S32 | S64 | S128
  deriving (Eq, Ord)

instance Show Size where
  show = \case
    S1   -> "bit"
    S8   -> "byte"
    S16  -> "word"
    S32  -> "dword"
    S64  -> "qword"
    S128 -> "oword"

mkSize  1 = S8
mkSize  2 = S16
mkSize  4 = S32
mkSize  8 = S64
mkSize 16 = S128

sizeLen = \case
  S8   -> 1
  S16  -> 2
  S32  -> 4
  S64  -> 8
  S128 -> 16

class HasSize a where size :: a -> Size

instance HasSize Word8  where size _ = S8
instance HasSize Word16 where size _ = S16
instance HasSize Word32 where size _ = S32
instance HasSize Word64 where size _ = S64
instance HasSize Int8   where size _ = S8
instance HasSize Int16  where size _ = S16
instance HasSize Int32  where size _ = S32
instance HasSize Int64  where size _ = S64

-- | Singleton type for size
data TypedSize (s :: Size) where
  TypedSize1   :: TypedSize S1
  TypedSize8   :: TypedSize S8
  TypedSize16  :: TypedSize S16
  TypedSize32  :: TypedSize S32
  TypedSize64  :: TypedSize S64
  TypedSize128 :: TypedSize S128

instance HasSize (TypedSize s) where
  size = \case
    TypedSize1   -> S1
    TypedSize8   -> S8
    TypedSize16  -> S16
    TypedSize32  -> S32
    TypedSize64  -> S64
    TypedSize128 -> S128

class WithTypedSize (s :: Size) where
  ssize :: TypedSize s

instance WithTypedSize S1   where ssize = TypedSize1
instance WithTypedSize S8   where ssize = TypedSize8
instance WithTypedSize S16  where ssize = TypedSize16
instance WithTypedSize S32  where ssize = TypedSize32
instance WithTypedSize S64  where ssize = TypedSize64
instance WithTypedSize S128 where ssize = TypedSize128

data EqT s s' where
  Refl :: EqT s s

sizeEqCheck :: forall s s' f g . (WithTypedSize s, WithTypedSize s') => f s -> g s' -> Maybe (EqT s s')
sizeEqCheck _ _ = case (ssize :: TypedSize s, ssize :: TypedSize s') of
  (TypedSize8 , TypedSize8)  -> Just Refl
  (TypedSize16, TypedSize16) -> Just Refl
  (TypedSize32, TypedSize32) -> Just Refl
  (TypedSize64, TypedSize64) -> Just Refl
  _ -> Nothing