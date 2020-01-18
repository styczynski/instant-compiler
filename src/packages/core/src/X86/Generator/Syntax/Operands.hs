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
module X86.Generator.Syntax.Operands where

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

import X86.Generator.Syntax.Bytes
import X86.Generator.Syntax.Sizes
import X86.Generator.Syntax.Scale
import X86.Generator.Syntax.Utils
import X86.Generator.Syntax.Registers

data Immediate a
  = Immediate a
  | LabelRelValue Size{-size hint-} Label

-- Type of labels
newtype Label = Label {unLabel :: Int}

-- | Operand access modes
data AccessMode
  = AccessReadOnly     -- ^ readable operand
  | AccessReadWrite    -- ^ readable and writeable operand

-- | An operand can be an immediate, a register, a memory address or RIP-relative (memory address relative to the instruction pointer)
data Operand :: AccessMode -> Size -> * where
  ImmOp     :: Immediate Int64 -> Operand AccessReadOnly s
  RegOp     :: Reg s -> Operand rw s
  MemOp     :: WithTypedSize s' => RelativeAddress s' -> Operand rw s
  IPMemOp   :: Immediate Int32 -> Operand rw s

instance Show a => Show (Immediate a) where
  show (Immediate x) = show x
  show (LabelRelValue s x) = show x

instance Show Label where
  show (Label i) = ".l" ++ show i

instance FromReg (Operand r) where
  fromReg = RegOp

instance WithTypedSize s => Show (Operand a s) where
  show = \case
    ImmOp w       -> show w
    RegOp r       -> show r
    r@(MemOp   a) -> show (size r) ++ " [" ++ show a ++ "]"
    r@(IPMemOp x) -> show (size r) ++ " [" ++ "rel " ++ show x ++ "]"
   where
    showp x | x < 0 = " - " ++ show (-x)
    showp x         = " + " ++ show x

instance WithTypedSize s => HasSize (Operand a s) where
  size _ = size (getSizeOf :: TypedSize s)

instance (rw ~ AccessReadOnly) => Num (Operand rw s) where
  negate (ImmOp (Immediate x)) = ImmOp $ Immediate $ negate x
  fromInteger (Integral x) = ImmOp $ Immediate x
  fromInteger z = error $ show z ++ " does not fit into " -- ++ show s
  (+) = error "(+) @Operand"
  (-) = error "(-) @Operand"
  (*) = error "(*) @Operand"
  abs = error "abs @Operand"
  signum = error "signum @Operand"

-- | intruction pointer (RIP) relative address
ipRel :: Label -> Operand rw s
ipRel l = IPMemOp $ LabelRelValue Size32B l

ipRelValue l = ImmOp $ LabelRelValue Size32B l

-- | `ipRel` with specialized type
ipRel8 :: Label -> Operand rw Size8B
ipRel8 = ipRel

addr :: WithTypedSize s => Address s -> Operand rw s'
addr = MemOp . makeRelativeAddress

-- | `addr` with specialized type
addr8 :: WithTypedSize s => Address s -> Operand rw Size8B
addr8 = addr

-- | `addr` with specialized type
addr16 :: WithTypedSize s => Address s -> Operand rw Size16B
addr16 = addr

-- | `addr` with specialized type
addr32 :: WithTypedSize s => Address s -> Operand rw Size32B
addr32 = addr

-- | `addr` with specialized type
addr64 :: WithTypedSize s => Address s -> Operand rw Size64B
addr64 = addr

rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: FromReg c => c Size64B
rax  = reg (RegName "rax") 0x0
rcx  = reg (RegName "rcx") 0x1
rdx  = reg (RegName "rdx") 0x2
rbx  = reg (RegName "rbx") 0x3
rsp  = reg (RegName "rsp") 0x4
rbp  = reg (RegName "rbp") 0x5
rsi  = reg (RegName "rsi") 0x6
rdi  = reg (RegName "rdi") 0x7
r8   = reg (RegName "r8") 0x8
r9   = reg (RegName "r9") 0x9
r10  = reg (RegName "r10") 0xa
r11  = reg (RegName "r11") 0xb
r12  = reg (RegName "r12") 0xc
r13  = reg (RegName "r13") 0xd
r14  = reg (RegName "r14") 0xe
r15  = reg (RegName "r15") 0xf

eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d :: FromReg c => c Size32B
eax  = reg (RegName "eax") 0x0
ecx  = reg (RegName "ecx") 0x1
edx  = reg (RegName "edx") 0x2
ebx  = reg (RegName "ebx") 0x3
esp  = reg (RegName "esp") 0x4
ebp  = reg (RegName "ebp") 0x5
esi  = reg (RegName "esi") 0x6
edi  = reg (RegName "edi") 0x7
r8d  = reg (RegName "r8d") 0x8
r9d  = reg (RegName "r9d") 0x9
r10d = reg (RegName "r10d") 0xa
r11d = reg (RegName "r11d") 0xb
r12d = reg (RegName "r12d") 0xc
r13d = reg (RegName "r13d") 0xd
r14d = reg (RegName "r14d") 0xe
r15d = reg (RegName "r15d") 0xf

ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w :: FromReg c => c Size16B
ax   = reg (RegName "ax") 0x0
cx   = reg (RegName "cx") 0x1
dx   = reg (RegName "dx") 0x2
bx   = reg (RegName "bx") 0x3
sp   = reg (RegName "sp") 0x4
bp   = reg (RegName "bp") 0x5
si   = reg (RegName "si") 0x6
di   = reg (RegName "di") 0x7
r8w  = reg (RegName "r8w") 0x8
r9w  = reg (RegName "r9w") 0x9
r10w = reg (RegName "r10w") 0xa
r11w = reg (RegName "r11w") 0xb
r12w = reg (RegName "r12w") 0xc
r13w = reg (RegName "r13w") 0xd
r14w = reg (RegName "r14w") 0xe
r15w = reg (RegName "r15w") 0xf

al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b :: FromReg c => c Size8B
al   = reg (RegName "al") 0x0
cl   = reg (RegName "cl") 0x1
dl   = reg (RegName "dl") 0x2
bl   = reg (RegName "bl") 0x3
spl  = reg (RegName "spl") 0x4
bpl  = reg (RegName "bpl") 0x5
sil  = reg (RegName "sil") 0x6
dil  = reg (RegName "dil") 0x7
r8b  = reg (RegName "r8b") 0x8
r9b  = reg (RegName "r9b") 0x9
r10b = reg (RegName "r10b") 0xa
r11b = reg (RegName "r11b") 0xb
r12b = reg (RegName "r12b") 0xc
r13b = reg (RegName "r13b") 0xd
r14b = reg (RegName "r14b") 0xe
r15b = reg (RegName "r15b") 0xf

ah, ch, dh, bh :: FromReg c => c Size8B
ah   = fromReg $ HighReg (RegName "ah") 0x0
ch   = fromReg $ HighReg (RegName "ch") 0x1
dh   = fromReg $ HighReg (RegName "dh") 0x2
bh   = fromReg $ HighReg (RegName "bh") 0x3

xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 :: FromReg c => c Size128B
xmm0 = fromReg $ XMM (RegName "xmm0") 0x0
xmm1 = fromReg $ XMM (RegName "xmm1") 0x1
xmm2 = fromReg $ XMM (RegName "xmm2") 0x2
xmm3 = fromReg $ XMM (RegName "xmm3") 0x3
xmm4 = fromReg $ XMM (RegName "xmm4") 0x4
xmm5 = fromReg $ XMM (RegName "xmm5") 0x5
xmm6 = fromReg $ XMM (RegName "xmm6") 0x6
xmm7 = fromReg $ XMM (RegName "xmm7") 0x7

pattern RegA = RegOp (NormalReg (RegNameUnknown) 0x0)

pattern RegCl :: Operand r Size8B
pattern RegCl = RegOp (NormalReg (RegNameUnknown) 0x1)

--------------------------------------------------------------

resizeOperand :: WithTypedSize s' => Operand AccessReadWrite s -> Operand AccessReadWrite s'
resizeOperand (RegOp x) = RegOp $ resizeRegCode x
resizeOperand (MemOp a) = MemOp a
resizeOperand (IPMemOp a) = IPMemOp a

resizeRegCode :: Reg s -> Reg s'
resizeRegCode (NormalReg r i) = NormalReg r i

pattern MemLike <- (isMemOp -> True)

isMemOp MemOp{} = True
isMemOp IPMemOp{} = True
isMemOp _ = False