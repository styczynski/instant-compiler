{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language RecursiveDo #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language CPP #-}
module X86.Generator.CodeGen where

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
import X86.Generator.LCode
import X86.Generator.Environment
import X86.Generator.Registers

ret        = createCodeLine Ret_
nop        = createCodeLine Nop_
pushf      = createCodeLine PushF_
popf       = createCodeLine PopF_
cmc        = createCodeLine Cmc_
clc        = createCodeLine Clc_
stc        = createCodeLine Stc_
cli        = createCodeLine Cli_
sti        = createCodeLine Sti_
cld        = createCodeLine Cld_
std        = createCodeLine Std_
inc a      = createCodeLine (Inc_ a)
dec a      = createCodeLine (Dec_ a)
not_ a     = createCodeLine (Not_ a)
neg a      = createCodeLine (Neg_ a)
bswap a    = createCodeLine (Bswap a)
bsf a b    = createCodeLine (Bsf a b)
bsr a b    = createCodeLine (Bsr a b)
bt a b     = createCodeLine (Bt  a b)
add a b    = createCodeLine (Add_ a b)
or_  a b   = createCodeLine (Or_  a b)
adc a b    = createCodeLine (Adc_ a b)
sbb a b    = createCodeLine (Sbb_ a b)
and_ a b   = createCodeLine (And_ a b)
sub a b    = createCodeLine (Sub_ a b)
xor_ a b   = createCodeLine (Xor_ a b)
cmp a b    = createCodeLine (Cmp_ a b)
test a b   = createCodeLine (Test_ a b)
mov a b    = createCodeLine (Mov_ a b)

mov' :: forall s s' r . IsSize s' => Operand RW s -> Operand r s' -> Code LCode CodeLine
mov' a = mov (resizeOperand a :: Operand RW s')

cmov c a b = createCodeLine (Cmov_ c a b)
rol a b    = createCodeLine (Rol_ a b)
ror a b    = createCodeLine (Ror_ a b)
rcl a b    = createCodeLine (Rcl_ a b)
rcr a b    = createCodeLine (Rcr_ a b)
shl a b    = createCodeLine (Shl_ a b)
shr a b    = createCodeLine (Shr_ a b)
sar a b    = createCodeLine (Sar_ a b)
xchg a b   = createCodeLine (Xchg_ a b)
movd   a b = createCodeLine (Movd_   a b)
movq   a b = createCodeLine (Movq_   a b)
movdqa a b = createCodeLine (Movdqa_ a b)
paddb  a b = createCodeLine (Paddb_  a b)
paddw  a b = createCodeLine (Paddw_  a b)
paddd  a b = createCodeLine (Paddd_  a b)
paddq  a b = createCodeLine (Paddq_  a b)
psubb  a b = createCodeLine (Psubb_  a b)
psubw  a b = createCodeLine (Psubw_  a b)
psubd  a b = createCodeLine (Psubd_  a b)
psubq  a b = createCodeLine (Psubq_  a b)
pxor   a b = createCodeLine (Pxor_   a b)
psllw  a b = createCodeLine (Psllw_  a b)
pslld  a b = createCodeLine (Pslld_  a b)
psllq  a b = createCodeLine (Psllq_  a b)
pslldq a b = createCodeLine (Pslldq_ a b)
psrlw  a b = createCodeLine (Psrlw_  a b)
psrld  a b = createCodeLine (Psrld_  a b)
psrlq  a b = createCodeLine (Psrlq_  a b)
psrldq a b = createCodeLine (Psrldq_ a b)
psraw  a b = createCodeLine (Psraw_  a b)
psrad  a b = createCodeLine (Psrad_  a b)
lea a b    = createCodeLine (Lea_ a b)
j a c      = createCodeLine (J_ a Nothing c)
pop a      = createCodeLine (Pop_ a)
push a     = createCodeLine (Push_ a)
call a     = createCodeLine (Call_ a)
jmpq a     = createCodeLine (Jmpq_ a)
jmp b      = createCodeLine (Jmp_ Nothing b)
db a       = createCodeLine (Data_ a)
align a    = createCodeLine (Align_ a)

incrementLabelPtr :: CodeState -> CodeState
incrementLabelPtr s = s { labelPtr = (labelPtr s)+1 }

incrementRegPtr :: CodeState -> CodeState
incrementRegPtr s = s { regPtr = (regPtr s)+1 }

label :: (BuilderCode c CodeLine) => CodeM c CodeLine Label
label = do
  s <- CodeM get
  CodeM $ put $ incrementLabelPtr s
  createCodeLine Label_
  return $ Label $ labelPtr s

