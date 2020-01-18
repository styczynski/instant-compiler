{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
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
module X86.Generator.LCode where

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
import X86.Generator.Environment
import X86.Generator.Registers

data LCode where
  Prebuilt   :: V.Vector Word8 -> LCode -> LCode
  EmptyCode  :: LCode
  AppendCode :: CodeBuilder -> LCode -> LCode -> LCode
  CodeLine   :: CodeBuilder -> CodeLine -> LCode

instance BuilderCode LCode CodeLine where
  codePrebuilt = Prebuilt
  emptyCodeInstr = EmptyCode
  appendCodeInstr = AppendCode
  codeLineInstr = CodeLine
  createCodeBuilder = mkCodeBuilder
  createCodeBuilder' _ = mkCodeBuilder'
  emptyCodeLine = ASMInstrLabel

instance Semigroup LCode where
  a <> b = AppendCode (mkCodeBuilder a <> mkCodeBuilder b) a b

instance Monoid LCode where
  mempty  = EmptyCode

mkCodeBuilder :: LCode -> CodeBuilder
mkCodeBuilder = \case
  CodeLine x _ -> x
  Prebuilt v _ -> mkCodeBuilder' (ASMInstrAlign 4) <> codeBytes (V.toList v)
  AppendCode x _ _ -> x
  EmptyCode -> mempty

instance Show LCode where
  show c = unlines $ zipWith3 showLine is (takes (zipWith (-) (tail is ++ [s]) is) bs) ss where
    ss = snd . runWriter . flip evalStateT 0 . showCode $ c
    (x, s) = buildCode c
    bs = V.toList $ compactCode (x, s)
    is = [i | Left i <- x]

    showLine addr [] s = s
    showLine addr bs s = [showNibble i addr | i <- [5,4..0]] ++ " " ++ pad (2 * maxbytes) (concatMap showByte bs) ++ " " ++ s

    pad i xs = xs ++ replicate (i - length xs) ' '

    maxbytes = 12


mkCodeBuilder' :: CodeLine -> CodeBuilder
mkCodeBuilder' = \case
  ASMInstrAdd  a b -> op2 0x0 a b
  ASMInstrOr   a b -> op2 0x1 a b
  ASMInstrAdc  a b -> op2 0x2 a b
  ASMInstrSbb  a b -> op2 0x3 a b
  ASMInstrAnd  a b -> op2 0x4 a b
  ASMInstrSub  a b -> op2 0x5 a b
  ASMInstrXor  a b -> op2 0x6 a b
  ASMInstrCmp  a b -> op2 0x7 a b

  ASMInstrRol a b -> shiftOp 0x0 a b
  ASMInstrRor a b -> shiftOp 0x1 a b
  ASMInstrRcl a b -> shiftOp 0x2 a b
  ASMInstrRcr a b -> shiftOp 0x3 a b
  ASMInstrShl a b -> shiftOp 0x4 a b -- sal
  ASMInstrShr a b -> shiftOp 0x5 a b
  ASMInstrSar a b -> shiftOp 0x7 a b

  ASMInstrXchg x@RegA r -> exchange_arg r
  ASMInstrXchg r x@RegA -> exchange_arg r
  ASMInstrXchg dest src -> op2' 0x43 dest' src where
    (dest', src') = if isMemOp src then (src, dest) else (dest, src)

  ASMInstrTest dest (mkImmNo64 (size dest) -> FJust (_, im)) -> case dest of
    RegA -> regprefix'' dest 0x54 mempty im
    _ -> regprefix'' dest 0x7b (reg8 0x0 dest) im
  ASMInstrTest dest (noImm "" -> src) -> op2' 0x42 dest' src' where
    (dest', src') = if isMemOp src then (src, dest) else (dest, src)

  ASMInstrMov dest@(RegOp r) ((if size dest == S64 then mkImmU S32 <> mkImm S64 else mkImm (size dest)) -> FJust ((se, si), im))
    | (se, si, size dest) /= (True, S32, S64) -> regprefix si dest (oneReg (0x16 .|. indicator (size dest /= S8)) r) im
    | otherwise -> regprefix'' dest 0x63 (reg8 0x0 dest) im
  ASMInstrMov dest@(size -> s) (mkImmNo64 s -> FJust (_, im)) -> regprefix'' dest 0x63 (reg8 0x0 dest) im
  ASMInstrMov dest src -> op2' 0x44 dest $ noImm (show (dest, src)) src

  ASMInstrCmov (Condition c) dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte (0x40 .|. c) <> reg2x8 dest src
  ASMInstrBsf dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbc <> reg2x8 dest src
  ASMInstrBsr dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbd <> reg2x8 dest src
  ASMInstrBt  src dest | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xa3 <> reg2x8 dest src

  ASMInstrLea dest src | size dest /= S8 -> regprefix2' (resizeOperand' dest src) dest 0x46 $ reg2x8 dest src where
    resizeOperand' :: WithTypedSize s1 => Operand x s1 -> Operand RW s2 -> Operand RW s1
    resizeOperand' _ = resizeOperand

  ASMInstrNot  a -> op1 0x7b 0x2 a
  ASMInstrNeg  a -> op1 0x7b 0x3 a
  ASMInstrInc  a -> op1 0x7f 0x0 a
  ASMInstrDec  a -> op1 0x7f 0x1 a
  ASMInstrBswap a@RegOp{} | size a >= S32 -> op1 0x07 0x1 a
  ASMInstrBswap a  -> error $ "wrong bswap operand: " ++ show a

  ASMInstrCall (ImmOp (LabelRelValue S32 l)) -> codeByte 0xe8 <> mkRef S32 4 l
  ASMInstrCall a -> op1' 0xff 0x2 a

  ASMInstrMovd a@OpXMM b -> sse 0x6e a b
  ASMInstrMovd b a@OpXMM -> sse 0x7e a b
  ASMInstrMovq b a@OpXMM -> sse 0xd6 a b
  ASMInstrMovdqa a@OpXMM b -> sse 0x6f a b
  ASMInstrMovdqa b a@OpXMM -> sse 0x7f a b
  ASMInstrPaddb  a b -> sse 0xfc a b
  ASMInstrPaddw  a b -> sse 0xfd a b
  ASMInstrPaddd  a b -> sse 0xfe a b
  ASMInstrPaddq  a b -> sse 0xd4 a b
  ASMInstrPsubb  a b -> sse 0xf8 a b
  ASMInstrPsubw  a b -> sse 0xf9 a b
  ASMInstrPsubd  a b -> sse 0xfa a b
  ASMInstrPsubq  a b -> sse 0xfb a b
  ASMInstrPxor   a b -> sse 0xef a b
  ASMInstrPsllw  a b -> sseShift 0x71 0x2 0xd1 a b
  ASMInstrPslld  a b -> sseShift 0x72 0x2 0xd2 a b
  ASMInstrPsllq  a b -> sseShift 0x73 0x2 0xd3 a b
  ASMInstrPslldq a b -> sseShift 0x73 0x7 undefined a b
  ASMInstrPsrlw  a b -> sseShift 0x71 0x6 0xf1 a b
  ASMInstrPsrld  a b -> sseShift 0x72 0x6 0xf2 a b
  ASMInstrPsrlq  a b -> sseShift 0x73 0x6 0xf3 a b
  ASMInstrPsrldq a b -> sseShift 0x73 0x3 undefined a b
  ASMInstrPsraw  a b -> sseShift 0x71 0x4 0xe1 a b
  ASMInstrPsrad  a b -> sseShift 0x72 0x4 0xe2 a b

  ASMInstrPop dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0b r) mempty
  ASMInstrPop dest -> regprefix S32 dest (codeByte 0x8f <> reg8 0x0 dest) mempty

  ASMInstrPush (mkImmS S8 -> FJust (_, im)) -> codeByte 0x6a <> im
  ASMInstrPush (mkImm S32 -> FJust (_, im)) -> codeByte 0x68 <> im
  ASMInstrPush dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0a r) mempty
  ASMInstrPush dest -> regprefix S32 dest (codeByte 0xff <> reg8 0x6 dest) mempty

  ASMInstrRet   -> codeByte 0xc3
  ASMInstrNop   -> codeByte 0x90
  ASMInstrPushF -> codeByte 0x9c
  ASMInstrPopF  -> codeByte 0x9d
  ASMInstrCmc   -> codeByte 0xf5
  ASMInstrClc   -> codeByte 0xf8
  ASMInstrStc   -> codeByte 0xf9
  ASMInstrCli   -> codeByte 0xfa
  ASMInstrSti   -> codeByte 0xfb
  ASMInstrCld   -> codeByte 0xfc
  ASMInstrStd   -> codeByte 0xfd

  ASMInstrJ (Condition c) (Just S8)  l -> codeByte (0x70 .|. c) <> mkRef S8 1 l
  ASMInstrJ (Condition c) (Just S32) l -> codeByte 0x0f <> codeByte (0x80 .|. c) <> mkRef S32 4 l
  ASMInstrJ (Condition c) Nothing  l -> mkAutoRef [(S8, [0x70 .|. c]), (S32, [0x0f, 0x80 .|. c])] l

  ASMInstrJmp (Just S8)  l -> codeByte 0xeb <> mkRef S8 1 l
  ASMInstrJmp (Just S32) l -> codeByte 0xe9 <> mkRef S32 4 l
  ASMInstrJmp Nothing  l -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l

  ASMInstrJmpq (ImmOp (LabelRelValue S32 l)) -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l
  ASMInstrJmpq a -> op1' 0xff 0x4 a

  ASMInstrLabel -> CodeBuilder 0 0 $ do
    bs <- lift $ mdo
      (n, ls, ps) <- getPast
      sendFuture (n, n: ls, ps')
      sendPast (ma, ma: mls)
      ~(ma, mls) <- getFuture
      let (bs, ps') = case ps of
            [] -> ([], [])
            corr: ps -> (concatMap g corr, ps)
          g (size, p, v) = zip [p..] $ case (size, v + n) of
            (S8, Integral v) -> toBytes (v :: Int8)
            (S32, Integral v) -> toBytes (v :: Int32)
            (s, i) -> error $ show i ++ " doesn't fit into " ++ show s
      return bs
    tell $ Right <$> bs


  ASMInstrData x -> codeBytes x

  ASMInstrAlign s -> CodeBuilder 0 (s-1) $ do
    bs <- lift $ mdo
      (n, ls, ps) <- getPast
      sendFuture (n', ls, ps)
      sendPast (ma + s-1, mls)
      ~(ma, mls) <- getFuture
      let n' = fromIntegral $ ((fromIntegral n - 1 :: Int64) .|. (fromIntegral s - 1)) + 1
      return $ zip [n..] $ nops $ n' - n
    tell $ Right <$> bs

  where
  convertImm :: Bool{-signed-} -> Size -> Operand r s -> First ((Bool, Size), CodeBuilder)
  convertImm a b (ImmOp (Immediate c)) = First $ (,) (a, b) . codeBytes <$> integralToBytes a b c
  convertImm True b (ImmOp (LabelRelValue s d)) | b == s = FJust $ (,) (True, b) $ mkRef s (sizeLen s) d
  convertImm _ _ _ = FNothing

  mkImmS, mkImmU, mkImm, mkImmNo64 :: Size -> Operand r s -> First ((Bool, Size), CodeBuilder)
  mkImmS = convertImm True
  mkImmU = convertImm False
  mkImm s = mkImmS s <> mkImmU s
  mkImmNo64 s = mkImm (no64 s)

  exchange_arg :: WithTypedSize s => Operand r s -> CodeBuilder
  exchange_arg dest@(RegOp r) | size dest /= S8 = regprefix (size dest) dest (oneReg 0x12 r) mempty
  exchange_arg dest = regprefix'' dest 0x43 (reg8 0x0 dest) mempty

  toCode :: HasBytes a => a -> CodeBuilder
  toCode = codeBytes . toBytes

  prefixSize :: [SReg] -> Size -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
  prefixSize rs s r x c im
    | noHighRex rs = pre <> c <> displacement r <> im
    | otherwise = error "cannot use high register in rex instruction"
    where
      pre = case s of
        S8  -> mem32pre r <> maybePrefix40
        S16 -> codeByte 0x66 <> mem32pre r <> prefix40 x
        S32 -> mem32pre r <> prefix40 x
        S64 -> mem32pre r <> prefix40 (0x8 .|. x)
        S128 -> mem32pre r <> codeByte 0x66 <> maybePrefix40

      mem32pre :: Operand r s -> CodeBuilder
      mem32pre (MemOp r@Addr{}) | size r == S32 = codeByte 0x67
      mem32pre _ = mempty

      prefix40 x = iff (x /= 0) $ prefixSize40 x
      prefixSize40 x = codeByte $ 0x40 .|. x

      maybePrefix40 = iff (any isRex rs || x /= 0) (prefixSize40 x)

      displacement :: Operand r s -> CodeBuilder
      displacement (IPMemOp (Immediate d)) = toCode d
      displacement (IPMemOp (LabelRelValue s@S32 d)) = mkRef s (sizeLen s + fromIntegral (codeBuilderLength im)) d
      displacement (MemOp (Addr b d i)) = mkSIB b i <> dispVal b d
        where
          mkSIB _ (IndexReg s (NormalReg n 0x4)) = error ("sp cannot be used as index: register " ++ show n)
          mkSIB _ (IndexReg s i) = f s $ regSize8 i
          mkSIB Nothing _ = f s1 0x4
          mkSIB (Just (regSize8 -> 0x4)) _ = f s1 0x4
          mkSIB _ _ = mempty

          f (Scale s) i = codeByte $ s `shiftL` 6 .|. i `shiftL` 3 .|. maybe 0x5 regSize8 b

          dispVal Just{} (Disp (Integral (d :: Int8))) = toCode d
          dispVal _ (Disp d) = toCode d
          dispVal Nothing _ = toCode (0 :: Int32)    -- [rbp] --> [rbp + 0]
          dispVal (Just (regSize8 -> 0x5)) _ = codeByte 0    -- [rbp] --> [rbp + 0]
          dispVal _ _ = mempty
      displacement _ = mempty

  regSize8 :: Reg t -> Word8
  regSize8 (NormalReg _ r) = r .&. 0x7
  regSize8 (HighReg _ r) = r .|. 0x4
  regSize8 (XMM _ r) = r .&. 0x7

  regprefix :: WithTypedSize s => Size -> Operand r s -> CodeBuilder -> CodeBuilder -> CodeBuilder
  regprefix s r = prefixSize (regs r) s r (extbits r)

  regprefix2 :: (WithTypedSize s1, WithTypedSize s) => Operand r1 s1 -> Operand r s -> CodeBuilder -> CodeBuilder
  regprefix2 r r' c = prefixSize (regs r <> regs r') (size r) r (extbits r' `shiftL` 2 .|. extbits r) c mempty

  regprefix'' :: WithTypedSize s => Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
  regprefix'' r p c = regprefix (size r) r $ extension r p <> c

  regprefix2' :: (WithTypedSize s1, WithTypedSize s) => Operand r1 s1 -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder
  regprefix2' r r' p c = regprefix2 r r' $ extension r p <> c

  sse :: WithTypedSize s => Word8 -> Operand r S128 -> Operand r' s -> CodeBuilder
  sse op a@OpXMM b = regprefix S128 b (codeByte 0x0f <> codeByte op <> reg2x8 a b) mempty

  sseShift :: Word8 -> Word8 -> Word8 -> Operand RW S128 -> Operand r S8 -> CodeBuilder
  sseShift op x op' a@OpXMM b@(mkImmU S8 -> FJust (_, i)) = regprefix S128 b (codeByte 0x0f <> codeByte op <> reg8 x a) i
  -- TODO: xmm argument

  extension :: HasSize a => a -> Word8 -> CodeBuilder
  extension x p = codeByte $ p `shiftL` 1 .|. indicator (size x /= S8)

  extbits :: Operand r s -> Word8
  extbits = \case
    MemOp (Addr b _ i) -> maybe 0 indexReg b .|. case i of NoIndex -> 0; IndexReg _ x -> indexReg x `shiftL` 1
    RegOp r -> indexReg r
    _ -> 0
    where
      indexReg (NormalReg _ r) = r `shiftR` 3 .&. 1
      indexReg _ = 0

  reg8 :: Word8 -> Operand r s -> CodeBuilder
  reg8 w x = codeByte $ operMode x `shiftL` 6 .|. w `shiftL` 3 .|. rc x
    where
      operMode :: Operand r s -> Word8
      operMode (MemOp (Addr (Just (regSize8 -> 0x5)) NoDisp _)) = 0x1   -- [rbp] --> [rbp + 0]
      operMode (MemOp (Addr Nothing _ _)) = 0x0
      operMode (MemOp (Addr _ NoDisp _))  = 0x0
      operMode (MemOp (Addr _ (Disp (Integral (_ :: Int8))) _))  = 0x1
      operMode (MemOp (Addr _ Disp{} _))  = 0x2
      operMode IPMemOp{}          = 0x0
      operMode _              = 0x3

      rc :: Operand r s -> Word8
      rc (MemOp (Addr (Just r) _ NoIndex)) = regSize8 r
      rc MemOp{}   = 0x04    -- SIB byte
      rc IPMemOp{} = 0x05
      rc (RegOp r) = regSize8 r

  op2 :: WithTypedSize s => Word8 -> Operand RW s -> Operand r s -> CodeBuilder
  op2 op dest@RegA src@(mkImmNo64 (size dest) -> FJust (_, im)) | size dest == S8 || isNothing (getFirst $ mkImmS S8 src)
    = regprefix'' dest (op `shiftL` 2 .|. 0x2) mempty im
  op2 op dest (mkImmS S8 <> mkImmNo64 (size dest) -> FJust ((_, k), im))
    = regprefix'' dest (0x40 .|. indicator (size dest /= S8 && k == S8)) (reg8 op dest) im
  op2 op dest src = op2' (op `shiftL` 2) dest $ noImm "1" src

  noImm :: String -> Operand r s -> Operand RW s
  noImm _ (RegOp r) = RegOp r
  noImm _ (MemOp a) = MemOp a
  noImm _ (IPMemOp a) = IPMemOp a
  noImm er _ = error $ "immediate value of this size is not supported: " ++ er

  op2' :: WithTypedSize s => Word8 -> Operand RW s -> Operand RW s -> CodeBuilder
  op2' op dest src@RegOp{} = op2g op dest src
  op2' op dest@RegOp{} src = op2g (op .|. 0x1) src dest

  op2g :: (WithTypedSize t, WithTypedSize s) => Word8 -> Operand r s -> Operand r' t -> CodeBuilder
  op2g op dest src = regprefix2' dest src op $ reg2x8 src dest

  reg2x8 :: (WithTypedSize s, WithTypedSize s') => Operand r s -> Operand r' s' -> CodeBuilder
  reg2x8 (RegOp r) = reg8 (regSize8 r)

  op1_ :: WithTypedSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder -> CodeBuilder
  op1_ r1 r2 dest = regprefix'' dest r1 (reg8 r2 dest)

  op1 :: WithTypedSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder
  op1 a b c = op1_ a b c mempty

  op1' :: Word8 -> Word8 -> Operand r S64 -> CodeBuilder
  op1' r1 r2 dest = regprefix S32 dest (codeByte r1 <> reg8 r2 dest) mempty

  shiftOp :: WithTypedSize s => Word8 -> Operand RW s -> Operand r S8 -> CodeBuilder
  shiftOp c dest (ImmOp (Immediate 1)) = op1 0x68 c dest
  shiftOp c dest (mkImmU S8 -> FJust (_, i)) = op1_ 0x60 c dest i
  shiftOp c dest RegCl = op1 0x69 c dest
  shiftOp _ _ _ = error "invalid shift operands"

  oneReg :: Word8 -> Reg t -> CodeBuilder
  oneReg x r = codeByte $ x `shiftL` 3 .|. regSize8 r

showCode = \case
  EmptyCode  -> return ()
  AppendCode _ a b -> showCode a >> showCode b
  Prebuilt _ c -> showCodeLine (ASMInstrAlign 4) >> codeLine "{" >> showCode c >> codeLine "}"
  CodeLine _ x -> showCodeLine x