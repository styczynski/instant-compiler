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
import X86.Generator.CodeBuilder
import X86.Generator.Environment

type X86 m = CodeM LCode CodeLine m

ret        = createCodeLine ASMInstrRet
nop        = createCodeLine ASMInstrNop
pushf      = createCodeLine ASMInstrPushF
popf       = createCodeLine ASMInstrPopF
cmc        = createCodeLine ASMInstrCmc
clc        = createCodeLine ASMInstrClc
stc        = createCodeLine ASMInstrStc
cli        = createCodeLine ASMInstrCli
sti        = createCodeLine ASMInstrSti
cld        = createCodeLine ASMInstrCld
std        = createCodeLine ASMInstrStd
inc a      = createCodeLine (ASMInstrInc a)
dec a      = createCodeLine (ASMInstrDec a)
not_ a     = createCodeLine (ASMInstrNot a)
neg a      = createCodeLine (ASMInstrNeg a)
bswap a    = createCodeLine (ASMInstrBswap a)
bsf a b    = createCodeLine (ASMInstrBsf a b)
bsr a b    = createCodeLine (ASMInstrBsr a b)
bt a b     = createCodeLine (ASMInstrBt  a b)
add a b    = createCodeLine (ASMInstrAdd a b)
or_  a b   = createCodeLine (ASMInstrOr  a b)
adc a b    = createCodeLine (ASMInstrAdc a b)
sbb a b    = createCodeLine (ASMInstrSbb a b)
and_ a b   = createCodeLine (ASMInstrAnd a b)
sub a b    = createCodeLine (ASMInstrSub a b)
xor_ a b   = createCodeLine (ASMInstrXor a b)
cmp a b    = createCodeLine (ASMInstrCmp a b)
test a b   = createCodeLine (ASMInstrTest a b)
mov a b    = createCodeLine (ASMInstrMov a b)

mov' :: forall s s' r . WithTypedSize s' => Operand AccessReadWrite s -> Operand r s' -> Code LCode CodeLine
mov' a = mov (resizeOperand a :: Operand AccessReadWrite s')

cmov c a b = createCodeLine (ASMInstrCmov c a b)
rol a b    = createCodeLine (ASMInstrRol a b)
ror a b    = createCodeLine (ASMInstrRor a b)
rcl a b    = createCodeLine (ASMInstrRcl a b)
rcr a b    = createCodeLine (ASMInstrRcr a b)
shl a b    = createCodeLine (ASMInstrShl a b)
shr a b    = createCodeLine (ASMInstrShr a b)
sar a b    = createCodeLine (ASMInstrSar a b)
xchg a b   = createCodeLine (ASMInstrXchg a b)
movd   a b = createCodeLine (ASMInstrMovd   a b)
movq   a b = createCodeLine (ASMInstrMovq   a b)
movdqa a b = createCodeLine (ASMInstrMovdqa a b)
paddb  a b = createCodeLine (ASMInstrPaddb  a b)
paddw  a b = createCodeLine (ASMInstrPaddw  a b)
paddd  a b = createCodeLine (ASMInstrPaddd  a b)
paddq  a b = createCodeLine (ASMInstrPaddq  a b)
psubb  a b = createCodeLine (ASMInstrPsubb  a b)
psubw  a b = createCodeLine (ASMInstrPsubw  a b)
psubd  a b = createCodeLine (ASMInstrPsubd  a b)
psubq  a b = createCodeLine (ASMInstrPsubq  a b)
pxor   a b = createCodeLine (ASMInstrPxor   a b)
psllw  a b = createCodeLine (ASMInstrPsllw  a b)
pslld  a b = createCodeLine (ASMInstrPslld  a b)
psllq  a b = createCodeLine (ASMInstrPsllq  a b)
pslldq a b = createCodeLine (ASMInstrPslldq a b)
psrlw  a b = createCodeLine (ASMInstrPsrlw  a b)
psrld  a b = createCodeLine (ASMInstrPsrld  a b)
psrlq  a b = createCodeLine (ASMInstrPsrlq  a b)
psrldq a b = createCodeLine (ASMInstrPsrldq a b)
psraw  a b = createCodeLine (ASMInstrPsraw  a b)
psrad  a b = createCodeLine (ASMInstrPsrad  a b)
lea a b    = createCodeLine (ASMInstrLea a b)
j a c      = createCodeLine (ASMInstrJ a Nothing c)
pop a      = createCodeLine (ASMInstrPop a)
push a     = createCodeLine (ASMInstrPush a)
call a     = createCodeLine (ASMInstrCall a)
jmpq a     = createCodeLine (ASMInstrJmpq a)
jmp b      = createCodeLine (ASMInstrJmp Nothing b)
db a       = createCodeLine (ASMInstrData a)
align a    = createCodeLine (ASMInstrAlign a)

incrementLabelPtr :: CodeState -> CodeState
incrementLabelPtr s = s { labelPtr = (labelPtr s)+1 }

incrementRegPtr :: CodeState -> CodeState
incrementRegPtr s = s { regPtr = (regPtr s)+1 }

label :: (BuilderCode c CodeLine) => CodeM c CodeLine Label
label = do
  s <- CodeM get
  CodeM $ put $ incrementLabelPtr s
  createCodeLine ASMInstrLabel
  return $ Label $ labelPtr s

