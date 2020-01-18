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
module X86.Generator.Syntax.Bytes where

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

type Bytes = [Word8]

class HasBytes a where toBytes :: a -> Bytes

instance HasBytes Word8  where
  toBytes w = [w]
instance HasBytes Word16 where
  toBytes w = [fromIntegral w, fromIntegral $ w `shiftR` 8]
instance HasBytes Word32 where
  toBytes w = [ fromIntegral $ w `shiftR` n | n <- [0, 8 .. 24] ]
instance HasBytes Word64 where
  toBytes w = [ fromIntegral $ w `shiftR` n | n <- [0, 8 .. 56] ]

instance HasBytes Int8  where
  toBytes w = toBytes (fromIntegral w :: Word8)
instance HasBytes Int16 where
  toBytes w = toBytes (fromIntegral w :: Word16)
instance HasBytes Int32 where
  toBytes w = toBytes (fromIntegral w :: Word32)
instance HasBytes Int64 where
  toBytes w = toBytes (fromIntegral w :: Word64)
