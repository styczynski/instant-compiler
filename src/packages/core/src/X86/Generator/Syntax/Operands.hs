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

-- | An operand can be an immediate, a register, a memory address or RIP-relative (memory address relative to the instruction pointer)
data Operand :: Access -> Size -> * where
  ImmOp     :: Immediate Int64 -> Operand R s
  RegOp     :: Reg s -> Operand rw s
  MemOp     :: WithTypedSize s' => Addr s' -> Operand rw s
  IPMemOp   :: Immediate Int32 -> Operand rw s

addr :: WithTypedSize s => Address s -> Operand rw s'
addr = MemOp . makeAddr

-- | `addr` with specialized type
addr8 :: WithTypedSize s => Address s -> Operand rw S8
addr8 = addr

-- | `addr` with specialized type
addr16 :: WithTypedSize s => Address s -> Operand rw S16
addr16 = addr

-- | `addr` with specialized type
addr32 :: WithTypedSize s => Address s -> Operand rw S32
addr32 = addr

-- | `addr` with specialized type
addr64 :: WithTypedSize s => Address s -> Operand rw S64
addr64 = addr

data Immediate a
  = Immediate a
  | LabelRelValue Size{-size hint-} Label

-- Type of labels
newtype Label = Label {unLabel :: Int}

instance Show Label where
  show (Label i) = ".l" ++ show i

-- | Operand access modes
data Access
  = R     -- ^ readable operand
  | RW    -- ^ readable and writeable operand

-- | Register name.
data RegName = RegName String | RegUnallocated String | RegNameUnknown
deriving instance Eq (RegName)
deriving instance Ord (RegName)

instance Show RegName where
  show (RegName name) = name
  show (RegUnallocated id) = "<unallocated:" ++ show id ++ ">"
  show RegNameUnknown = "<?>"

-- | A register.
data Reg :: Size -> * where
  NormalReg :: RegName -> Word8 -> Reg s      -- \"normal\" registers are for example @AL@, @BX@, @ECX@ or @RSI@
  HighReg   :: RegName -> Word8 -> Reg S8     -- \"high\" registers are @AH@, @BH@, @CH@ etc
  XMM       :: RegName -> Word8 -> Reg S128   -- XMM registers

deriving instance Eq (Reg s)
deriving instance Ord (Reg s)

-- | A (relative) address is made up base a base register, a displacement, and a (scaled) index.
-- For example in @[eax+4*ecx+20]@ the base register is @eax@, the displacement is @20@ and the
-- index is @4*ecx@.
data Addr s = Addr
  { baseReg        :: BaseReg s
  , displacement   :: Displacement
  , indexReg       :: IndexReg s
  }
  deriving (Eq)

type BaseReg s = Maybe (Reg s)

data IndexReg s = NoIndex | IndexReg Scale (Reg s)
  deriving (Eq)

type Displacement = Maybe Int32

pattern NoDisp = Nothing
pattern Disp a = Just a

-- | intruction pointer (RIP) relative address
ipRel :: Label -> Operand rw s
ipRel l = IPMemOp $ LabelRelValue S32 l

ipRelValue l = ImmOp $ LabelRelValue S32 l

-- | `ipRel` with specialized type
ipRel8 :: Label -> Operand rw S8
ipRel8 = ipRel

instance WithTypedSize s => Show (Reg s) where
  show (XMM _ i) = "xmm" ++ show i
  show (HighReg _ i) =
    (["ah", " ch", "dh", "bh"] ++ repeat (error ("show @Reg")))
      !! fromIntegral i

  show r@(NormalReg _ i) =
    (!! fromIntegral i) . (++ repeat (error ("show @Reg"))) $ case size r of
      S8 ->
        ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil"] ++ map (++ "b") r8
      S16 -> r0 ++ map (++ "w") r8
      S32 -> map ('e' :) r0 ++ map (++ "d") r8
      S64 -> map ('r' :) r0 ++ r8
   where
    r0 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
    r8 = ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

instance WithTypedSize s => Show (Addr s) where
  show (Addr b d i) = showSum $ shb b ++ shd d ++ shi i
   where
    shb Nothing  = []
    shb (Just x) = [(True, show x)]
    shd NoDisp   = []
    shd (Disp x) = [(signum x /= (-1), show (abs x))]
    shi NoIndex         = []
    shi (IndexReg sc x) = [(True, show' (scaleFactor sc) ++ show x)]
    show' 1 = ""
    show' n = show n ++ " * "
    showSum []                = "0"
    showSum ((True , x) : xs) = x ++ g xs
    showSum ((False, x) : xs) = "-" ++ x ++ g xs
    g = concatMap (\(a, b) -> f a ++ b)
    f True  = " + "
    f False = " - "

instance WithTypedSize s => Show (Operand a s) where
  show = \case
    ImmOp w       -> show w
    RegOp r       -> show r
    r@(MemOp   a) -> show (size r) ++ " [" ++ show a ++ "]"
    r@(IPMemOp x) -> show (size r) ++ " [" ++ "rel " ++ show x ++ "]"
   where
    showp x | x < 0 = " - " ++ show (-x)
    showp x         = " + " ++ show x

instance Show a => Show (Immediate a) where
  show (Immediate x) = show x
  show (LabelRelValue s x) = show x

instance WithTypedSize s => HasSize (Operand a s) where
  size _ = size (ssize :: TypedSize s)

instance WithTypedSize s => HasSize (Addr s) where
  size _ = size (ssize :: TypedSize s)

instance WithTypedSize s => HasSize (Address s) where
  size _ = size (ssize :: TypedSize s)

instance WithTypedSize s => HasSize (BaseReg s) where
  size _ = size (ssize :: TypedSize s)

instance WithTypedSize s => HasSize (Reg s) where
  size _ = size (ssize :: TypedSize s)

instance WithTypedSize s => HasSize (IndexReg s) where
  size _ = size (ssize :: TypedSize s)

instance (rw ~ R) => Num (Operand rw s) where
  negate (ImmOp (Immediate x)) = ImmOp $ Immediate $ negate x
  fromInteger (Integral x) = ImmOp $ Immediate x
  fromInteger z = error $ show z ++ " does not fit into " -- ++ show s
  (+) = error "(+) @Operand"
  (-) = error "(-) @Operand"
  (*) = error "(*) @Operand"
  abs = error "abs @Operand"
  signum = error "signum @Operand"

instance Semigroup (Addr s) where
  Addr a b c <> Addr a' b' c' = Addr (getFirst $ First a <> First a') (getFirst $ First b <> First b') (c <> c')

instance Semigroup (IndexReg s) where
  i <> NoIndex = i
  NoIndex <> i = i

instance Monoid (Addr s) where
  mempty = Addr (getFirst mempty) (getFirst mempty) mempty

instance Monoid (IndexReg s) where
  mempty = NoIndex

base :: Reg s -> Addr s
base x = Addr (Just x) NoDisp NoIndex

index :: Scale -> Reg s -> Addr s
index sc x = Addr Nothing NoDisp (IndexReg sc x)

index' :: Int -> Reg s -> Addr s
index' sc x = Addr Nothing NoDisp (IndexReg (toScale sc) x)

index1 = index s1
index2 = index s2
index4 = index s4
index8 = index s8

disp :: (Bits a, Integral a) => a -> Addr s
disp (Integral x)
  | x == 0 = mempty
  | otherwise = Addr Nothing (Disp x) NoIndex

data Address :: Size -> * where
  Address :: [(Int, Reg s)] -> Int -> Address s

scaleAddress :: (Int -> Int) -> Address s -> Address s
scaleAddress f (Address rs d) = Address (first f <$> rs) $ f d

instance Num (Address s) where
  fromInteger d = Address [] $ fromInteger d
  negate = scaleAddress negate

  Address [] t * a            = scaleAddress (t *) a
  a            * Address [] t = scaleAddress (t *) a

  Address rs d + Address rs' d' = Address (f rs rs') (d + d')   where
    f []              rs                  = rs
    f rs              []                  = rs
    f (p@(t, r) : rs) (p'@(t', r') : rs') = case compare r r' of
      LT -> p : f rs (p' : rs')
      GT -> p' : f (p : rs) rs'
      EQ | t + t' == 0 -> f rs rs'
         | otherwise   -> (t + t', r) : f rs rs'

  abs    = error "abs @Address"
  signum = error "signum @Address"

makeAddr :: Address s -> Addr s
makeAddr (Address [(1, r)] d) = base r <> disp d
makeAddr (Address [(t, r)] d) = index' t r <> disp d
makeAddr (Address [(1, r), (1, r'@(NormalReg _ 0x4))] d) = base r' <> index1 r <> disp d
makeAddr (Address [(1, r), (t, r')] d) = base r <> index' t r' <> disp d
makeAddr (Address [(t, r'), (1, r)] d) = base r <> index' t r' <> disp d

class FromReg c where
  fromReg :: Reg s -> c s

instance FromReg Reg where
  fromReg = id

instance FromReg (Operand r) where
  fromReg = RegOp

instance FromReg Address where
  fromReg r = Address [(1, r)] 0

reg a b = fromReg $ NormalReg a b

rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: FromReg c => c S64
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

eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d :: FromReg c => c S32
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

ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w :: FromReg c => c S16
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

al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b :: FromReg c => c S8
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

ah, ch, dh, bh :: FromReg c => c S8
ah   = fromReg $ HighReg (RegName "ah") 0x0
ch   = fromReg $ HighReg (RegName "ch") 0x1
dh   = fromReg $ HighReg (RegName "dh") 0x2
bh   = fromReg $ HighReg (RegName "bh") 0x3

xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 :: FromReg c => c S128
xmm0 = fromReg $ XMM (RegName "xmm0") 0x0
xmm1 = fromReg $ XMM (RegName "xmm1") 0x1
xmm2 = fromReg $ XMM (RegName "xmm2") 0x2
xmm3 = fromReg $ XMM (RegName "xmm3") 0x3
xmm4 = fromReg $ XMM (RegName "xmm4") 0x4
xmm5 = fromReg $ XMM (RegName "xmm5") 0x5
xmm6 = fromReg $ XMM (RegName "xmm6") 0x6
xmm7 = fromReg $ XMM (RegName "xmm7") 0x7

pattern RegA = RegOp (NormalReg (RegNameUnknown) 0x0)

pattern RegCl :: Operand r S8
pattern RegCl = RegOp (NormalReg (RegNameUnknown) 0x1)

--------------------------------------------------------------

resizeOperand :: WithTypedSize s' => Operand RW s -> Operand RW s'
resizeOperand (RegOp x) = RegOp $ resizeRegCode x
resizeOperand (MemOp a) = MemOp a
resizeOperand (IPMemOp a) = IPMemOp a

resizeRegCode :: Reg s -> Reg s'
resizeRegCode (NormalReg r i) = NormalReg r i

pattern MemLike <- (isMemOp -> True)

isMemOp MemOp{} = True
isMemOp IPMemOp{} = True
isMemOp _ = False