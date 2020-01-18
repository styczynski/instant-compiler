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
module X86.Generator.Syntax.Utils where

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

everyNth n [] = []
everyNth n xs = take n xs: everyNth n (drop n xs)

showNibble :: (Integral a, Bits a) => Int -> a -> Char
showNibble n x = toEnum (b + if b < 10 then 48 else 87)
  where b = fromIntegral $ x `shiftR` (4 * n) .&. 0x0f

showByte b = [showNibble 1 b, showNibble 0 b]

showHex' x = "0x" ++ showHex x ""

pattern Integral xs <- (toIntegralSized -> Just xs)