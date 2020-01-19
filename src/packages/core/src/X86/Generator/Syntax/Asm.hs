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
module X86.Generator.Syntax.Asm where

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
import X86.Generator.Syntax.Operands
import X86.Generator.Syntax.Utils


newtype Condition = Condition Word8

pattern O   = Condition 0x0
pattern NO  = Condition 0x1
pattern B   = Condition 0x2
pattern C   = Condition 0x2
pattern NB  = Condition 0x3
pattern NC  = Condition 0x3
pattern E   = Condition 0x4
pattern Z   = Condition 0x4
pattern NE  = Condition 0x5
pattern NZ  = Condition 0x5
pattern NA  = Condition 0x6
pattern BE  = Condition 0x6
pattern A   = Condition 0x7
pattern NBE = Condition 0x7
pattern S   = Condition 0x8
pattern NS  = Condition 0x9
pattern P   = Condition 0xa
pattern NP  = Condition 0xb
pattern L   = Condition 0xc
pattern NL  = Condition 0xd
pattern NG  = Condition 0xe
pattern LE  = Condition 0xe
pattern G   = Condition 0xf
pattern NLE = Condition 0xf

instance Show Condition where
  show (Condition x) = case x of
    0x0 -> "o"
    0x1 -> "no"
    0x2 -> "c"
    0x3 -> "nc"
    0x4 -> "z"
    0x5 -> "nz"
    0x6 -> "be"
    0x7 -> "nbe"
    0x8 -> "s"
    0x9 -> "ns"
    0xa -> "p"
    0xb -> "np"
    0xc -> "l"
    0xd -> "nl"
    0xe -> "le"
    0xf -> "nle"

pattern N cc <- (notCond -> cc)
  where N = notCond

notCond :: Condition -> Condition
notCond (Condition c) = Condition $ c `xor` 1

data CodeLine where
  ASMInstrRet     :: CodeLine
  ASMInstrNop     :: CodeLine
  ASMInstrPushF   :: CodeLine
  ASMInstrPopF    :: CodeLine
  ASMInstrCmc     :: CodeLine
  ASMInstrClc     :: CodeLine
  ASMInstrStc     :: CodeLine
  ASMInstrCli     :: CodeLine
  ASMInstrSti     :: CodeLine
  ASMInstrCld     :: CodeLine
  ASMInstrStd     :: CodeLine
  ASMInstrInc     :: WithTypedSize s => Operand AccessReadWrite s -> CodeLine
  ASMInstrDec     :: WithTypedSize s => Operand AccessReadWrite s -> CodeLine
  ASMInstrNot     :: WithTypedSize s => Operand AccessReadWrite s -> CodeLine
  ASMInstrNeg     :: WithTypedSize s => Operand AccessReadWrite s -> CodeLine
  ASMInstrBswap   :: WithTypedSize s => Operand AccessReadWrite s -> CodeLine
  ASMInstrAdd     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrIMul    :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrDiv     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrOr      :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrAdc     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrSbb     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrAnd     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrSub     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrXor     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrCmp     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrTest    :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrMov     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrBsf     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrBsr     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r s -> CodeLine
  ASMInstrRol     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrRor     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrRcl     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrRcr     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrShl     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrShr     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrSar     :: WithTypedSize s => Operand AccessReadWrite s -> Operand r Size8B -> CodeLine
  ASMInstrBt      :: WithTypedSize s => Operand r s -> Operand AccessReadWrite s -> CodeLine
  ASMInstrMovdqa  :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPaddb   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPaddw   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPaddd   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPaddq   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPsubb   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPsubw   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPsubd   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPsubq   :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPxor    :: Operand AccessReadWrite Size128B -> Operand r Size128B -> CodeLine
  ASMInstrPsllw   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPslld   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsllq   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPslldq  :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsrlw   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsrld   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsrlq   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsrldq  :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsraw   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrPsrad   :: Operand AccessReadWrite Size128B -> Operand r Size8B -> CodeLine
  ASMInstrMovd    :: (WithTypedSize s, WithTypedSize s') => Operand AccessReadWrite s -> Operand r s' -> CodeLine
  ASMInstrMovq    :: (WithTypedSize s, WithTypedSize s') => Operand AccessReadWrite s -> Operand r s' -> CodeLine
  ASMInstrCmov    :: WithTypedSize s => Condition -> Operand AccessReadWrite s -> Operand AccessReadWrite s -> CodeLine
  ASMInstrXchg    :: WithTypedSize s => Operand AccessReadWrite s -> Operand AccessReadWrite s -> CodeLine
  ASMInstrLea     :: (WithTypedSize s, WithTypedSize s') => Operand AccessReadWrite s -> Operand AccessReadWrite s' -> CodeLine
  ASMInstrPop     :: Operand AccessReadWrite Size64B -> CodeLine
  ASMInstrPush    :: Operand r  Size64B -> CodeLine
  ASMInstrCall    :: Operand r Size64B -> CodeLine
  ASMInstrJmpq    :: Operand r Size64B -> CodeLine
  ASMInstrJ       :: Condition -> Maybe Size -> Label -> CodeLine
  ASMInstrJmp     :: Maybe Size -> Label -> CodeLine
  ASMInstrLabel   :: CodeLine
  ASMInstrLabelStr:: String -> CodeLine
  ASMInstrData    :: Bytes -> CodeLine
  ASMInstrAlign   :: Int   -> CodeLine


newLabel = do
  i <- get
  put $ i + 1
  return $ Label i

codeLine x = tell [x]

showOp0 s = codeLine s
showOp s a = showOp0 $ s ++ " " ++ a
showOp1 s a = showOp s $ show a
showOp2 s a b = showOp s $ show a ++ ", " ++ show b

showCodeLine :: CodeLine -> StateT Int (Writer [String]) ()
showCodeLine = \case
  ASMInstrAdd   op1 op2 -> showOp2 "add"  op1 op2
  ASMInstrOr    op1 op2 -> showOp2 "or"   op1 op2
  ASMInstrAdc   op1 op2 -> showOp2 "adc"  op1 op2
  ASMInstrSbb   op1 op2 -> showOp2 "sbb"  op1 op2
  ASMInstrAnd   op1 op2 -> showOp2 "and"  op1 op2
  ASMInstrSub   op1 op2 -> showOp2 "sub"  op1 op2
  ASMInstrIMul  op1 op2 -> showOp2 "imul"  op1 op2
  ASMInstrDiv   op1 op2 -> showOp2 "div"  op1 op2
  ASMInstrXor   op1 op2 -> showOp2 "xor"  op1 op2
  ASMInstrCmp   op1 op2 -> showOp2 "cmp"  op1 op2
  ASMInstrTest  op1 op2 -> showOp2 "test" op1 op2
  ASMInstrBsf   op1 op2 -> showOp2 "bsf"  op1 op2
  ASMInstrBsr   op1 op2 -> showOp2 "bsr"  op1 op2
  ASMInstrBt    op1 op2 -> showOp2 "bt"   op1 op2
  ASMInstrRol   op1 op2 -> showOp2 "rol"  op1 op2
  ASMInstrRor   op1 op2 -> showOp2 "ror"  op1 op2
  ASMInstrRcl   op1 op2 -> showOp2 "rcl"  op1 op2
  ASMInstrRcr   op1 op2 -> showOp2 "rcr"  op1 op2
  ASMInstrShl   op1 op2 -> showOp2 "shl"  op1 op2
  ASMInstrShr   op1 op2 -> showOp2 "shr"  op1 op2
  ASMInstrSar   op1 op2 -> showOp2 "sar"  op1 op2
  ASMInstrMov   op1 op2 -> showOp2 "mov"  op1 op2
  ASMInstrCmov  cc op1 op2 -> showOp2 ("cmov" ++ show cc) op1 op2
  ASMInstrLea   op1 op2 -> showOp2 "lea"  op1 op2
  ASMInstrXchg  op1 op2 -> showOp2 "xchg" op1 op2
  ASMInstrMovd    op1 op2 -> showOp2 "movd"   op1 op2
  ASMInstrMovq    op1 op2 -> showOp2 "movq"   op1 op2
  ASMInstrMovdqa  op1 op2 -> showOp2 "movdqa" op1 op2
  ASMInstrPaddb   op1 op2 -> showOp2 "paddb"  op1 op2
  ASMInstrPaddw   op1 op2 -> showOp2 "paddw"  op1 op2
  ASMInstrPaddd   op1 op2 -> showOp2 "paddd"  op1 op2
  ASMInstrPaddq   op1 op2 -> showOp2 "paddq"  op1 op2
  ASMInstrPsubb   op1 op2 -> showOp2 "psubb"  op1 op2
  ASMInstrPsubw   op1 op2 -> showOp2 "psubw"  op1 op2
  ASMInstrPsubd   op1 op2 -> showOp2 "psubd"  op1 op2
  ASMInstrPsubq   op1 op2 -> showOp2 "psubq"  op1 op2
  ASMInstrPxor    op1 op2 -> showOp2 "pxor"   op1 op2
  ASMInstrPsllw   op1 op2 -> showOp2 "psllw"  op1 op2
  ASMInstrPslld   op1 op2 -> showOp2 "pslld"  op1 op2
  ASMInstrPsllq   op1 op2 -> showOp2 "psllq"  op1 op2
  ASMInstrPslldq  op1 op2 -> showOp2 "pslldq" op1 op2
  ASMInstrPsrlw   op1 op2 -> showOp2 "psrlw"  op1 op2
  ASMInstrPsrld   op1 op2 -> showOp2 "psrld"  op1 op2
  ASMInstrPsrlq   op1 op2 -> showOp2 "psrlq"  op1 op2
  ASMInstrPsrldq  op1 op2 -> showOp2 "psrldq" op1 op2
  ASMInstrPsraw   op1 op2 -> showOp2 "psraw"  op1 op2
  ASMInstrPsrad   op1 op2 -> showOp2 "psrad"  op1 op2
  ASMInstrInc   op -> showOp1 "inc"  op
  ASMInstrDec   op -> showOp1 "dec"  op
  ASMInstrNot   op -> showOp1 "not"  op
  ASMInstrNeg   op -> showOp1 "neg"  op
  ASMInstrBswap op -> showOp1 "bswap" op
  ASMInstrPop   op -> showOp1 "pop"  op
  ASMInstrPush  op -> showOp1 "push" op
  ASMInstrCall  op -> showOp1 "call" op
  ASMInstrJmpq  op -> showOp1 "jmp"  op
  ASMInstrRet    -> showOp0 "ret"
  ASMInstrNop    -> showOp0 "nop"
  ASMInstrPushF  -> showOp0 "pushf"
  ASMInstrPopF   -> showOp0 "popf"
  ASMInstrCmc    -> showOp0 "cmc"
  ASMInstrClc    -> showOp0 "clc"
  ASMInstrStc    -> showOp0 "stc"
  ASMInstrCli    -> showOp0 "cli"
  ASMInstrSti    -> showOp0 "sti"
  ASMInstrCld    -> showOp0 "cld"
  ASMInstrStd    -> showOp0 "std"

  ASMInstrAlign  s -> codeLine $ ".align " ++ show s
  ASMInstrData  x
      | 2 * length (filter isPrint x) > length x -> showOp "db" $ show (toEnum . fromIntegral <$> x :: String)
      | otherwise -> showOp "db" $ intercalate ", " (show <$> x)
    where
      isPrint c = c >= 32 && c <= 126

  ASMInstrJ  cc s l -> showOp ("j" ++ show cc) $ (case s of Just Size8B -> "short "; Just Size32B -> "near "; _ -> "") ++ show l
  ASMInstrJmp  s  l -> showOp "jmp" $ (case s of Just Size8B -> "short "; Just Size32B -> "near "; _ -> "") ++ show l
  ASMInstrLabel     -> newLabel >>= codeLine . show
  ASMInstrLabelStr s-> (codeLine . show) (LabelStr s)


