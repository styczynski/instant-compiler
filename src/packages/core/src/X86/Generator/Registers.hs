{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language GADTs #-}
module X86.Generator.Registers where

import Numeric
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Int
import Data.Word
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Tardis
import Debug.Trace

import qualified Data.Map                      as Map

import X86.Generator.Asm

data SReg where
  SReg :: WithTypedSize s => Reg s -> SReg

phisicalReg :: SReg -> Reg S64
phisicalReg (SReg (HighReg n x)) = NormalReg n x
phisicalReg (SReg (NormalReg n x)) = NormalReg n x

isHigh (SReg HighReg{}) = True
isHigh _ = False

regs :: WithTypedSize s => Operand r s -> [SReg]
regs = \case
  MemOp (Addr r _ i) -> foldMap (pure . SReg) r ++ case i of NoIndex -> []; IndexReg _ x -> [SReg x]
  RegOp r -> [SReg r]
  _ -> mempty

isRex (SReg x@(NormalReg _ r)) = r .&. 0x8 /= 0 || size x == S8 && r `shiftR` 2 == 1
isRex _ = False

noHighRex r = not $ any isHigh r && any isRex r

no64 S64 = S32
no64 s = s

