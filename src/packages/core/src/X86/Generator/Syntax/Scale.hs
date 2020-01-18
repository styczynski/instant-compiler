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
module X86.Generator.Syntax.Scale where

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


-- | The scaling of an index. (replace with Size?)
newtype Scale = Scale Word8
  deriving (Eq)

s1 = Scale 0x0
s2 = Scale 0x1
s4 = Scale 0x2
s8 = Scale 0x3

toScale = \case
  1 -> s1
  2 -> s2
  4 -> s4
  8 -> s8

scaleFactor (Scale i) = case i of
  0x0 -> 1
  0x1 -> 2
  0x2 -> 4
  0x3 -> 8