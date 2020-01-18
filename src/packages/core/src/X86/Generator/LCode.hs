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

instance BuilderCode (LCode) (CodeLine) where
  codePrebuilt = Prebuilt
  emptyCodeInstr = EmptyCode
  appendCodeInstr = AppendCode
  codeLineInstr = CodeLine
  createCodeBuilder = mkCodeBuilder
  createCodeBuilder' _ = mkCodeBuilder'
  emptyCodeLine = Label_
  --xshowCode = showCode
  --xlabel = Label_

instance Semigroup LCode where
  a <> b = AppendCode (mkCodeBuilder a <> mkCodeBuilder b) a b

instance Monoid LCode where
  mempty  = EmptyCode

mkCodeBuilder :: LCode -> CodeBuilder
mkCodeBuilder = \case
  CodeLine x _ -> x
  Prebuilt v _ -> mkCodeBuilder' (Align_ 4) <> codeBytes (V.toList v)
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
  Add_  a b -> op2 0x0 a b
  Or_   a b -> op2 0x1 a b
  Adc_  a b -> op2 0x2 a b
  Sbb_  a b -> op2 0x3 a b
  And_  a b -> op2 0x4 a b
  Sub_  a b -> op2 0x5 a b
  Xor_  a b -> op2 0x6 a b
  Cmp_  a b -> op2 0x7 a b

  Rol_ a b -> shiftOp 0x0 a b
  Ror_ a b -> shiftOp 0x1 a b
  Rcl_ a b -> shiftOp 0x2 a b
  Rcr_ a b -> shiftOp 0x3 a b
  Shl_ a b -> shiftOp 0x4 a b -- sal
  Shr_ a b -> shiftOp 0x5 a b
  Sar_ a b -> shiftOp 0x7 a b

  Xchg_ x@RegA r -> xchg_a r
  Xchg_ r x@RegA -> xchg_a r
  Xchg_ dest src -> op2' 0x43 dest' src where
    (dest', src') = if isMemOp src then (src, dest) else (dest, src)

  Test_ dest (mkImmNo64 (size dest) -> FJust (_, im)) -> case dest of
    RegA -> regprefix'' dest 0x54 mempty im
    _ -> regprefix'' dest 0x7b (reg8 0x0 dest) im
  Test_ dest (noImm "" -> src) -> op2' 0x42 dest' src' where
    (dest', src') = if isMemOp src then (src, dest) else (dest, src)

  Mov_ dest@(RegOp r) ((if size dest == S64 then mkImmU S32 <> mkImm S64 else mkImm (size dest)) -> FJust ((se, si), im))
    | (se, si, size dest) /= (True, S32, S64) -> regprefix si dest (oneReg (0x16 .|. indicator (size dest /= S8)) r) im
    | otherwise -> regprefix'' dest 0x63 (reg8 0x0 dest) im
  Mov_ dest@(size -> s) (mkImmNo64 s -> FJust (_, im)) -> regprefix'' dest 0x63 (reg8 0x0 dest) im
  Mov_ dest src -> op2' 0x44 dest $ noImm (show (dest, src)) src

  Cmov_ (Condition c) dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte (0x40 .|. c) <> reg2x8 dest src
  Bsf dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbc <> reg2x8 dest src
  Bsr dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbd <> reg2x8 dest src
  Bt  src dest | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xa3 <> reg2x8 dest src

  Lea_ dest src | size dest /= S8 -> regprefix2' (resizeOperand' dest src) dest 0x46 $ reg2x8 dest src where
    resizeOperand' :: IsSize s1 => Operand x s1 -> Operand RW s2 -> Operand RW s1
    resizeOperand' _ = resizeOperand

  Not_  a -> op1 0x7b 0x2 a
  Neg_  a -> op1 0x7b 0x3 a
  Inc_  a -> op1 0x7f 0x0 a
  Dec_  a -> op1 0x7f 0x1 a
  Bswap a@RegOp{} | size a >= S32 -> op1 0x07 0x1 a
  Bswap a  -> error $ "wrong bswap operand: " ++ show a

  Call_ (ImmOp (LabelRelValue S32 l)) -> codeByte 0xe8 <> mkRef S32 4 l
  Call_ a -> op1' 0xff 0x2 a

  Movd_ a@OpXMM b -> sse 0x6e a b
  Movd_ b a@OpXMM -> sse 0x7e a b
  Movq_ b a@OpXMM -> sse 0xd6 a b
  Movdqa_ a@OpXMM b -> sse 0x6f a b
  Movdqa_ b a@OpXMM -> sse 0x7f a b
  Paddb_  a b -> sse 0xfc a b
  Paddw_  a b -> sse 0xfd a b
  Paddd_  a b -> sse 0xfe a b
  Paddq_  a b -> sse 0xd4 a b
  Psubb_  a b -> sse 0xf8 a b
  Psubw_  a b -> sse 0xf9 a b
  Psubd_  a b -> sse 0xfa a b
  Psubq_  a b -> sse 0xfb a b
  Pxor_   a b -> sse 0xef a b
  Psllw_  a b -> sseShift 0x71 0x2 0xd1 a b
  Pslld_  a b -> sseShift 0x72 0x2 0xd2 a b
  Psllq_  a b -> sseShift 0x73 0x2 0xd3 a b
  Pslldq_ a b -> sseShift 0x73 0x7 undefined a b
  Psrlw_  a b -> sseShift 0x71 0x6 0xf1 a b
  Psrld_  a b -> sseShift 0x72 0x6 0xf2 a b
  Psrlq_  a b -> sseShift 0x73 0x6 0xf3 a b
  Psrldq_ a b -> sseShift 0x73 0x3 undefined a b
  Psraw_  a b -> sseShift 0x71 0x4 0xe1 a b
  Psrad_  a b -> sseShift 0x72 0x4 0xe2 a b

  Pop_ dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0b r) mempty
  Pop_ dest -> regprefix S32 dest (codeByte 0x8f <> reg8 0x0 dest) mempty

  Push_ (mkImmS S8 -> FJust (_, im)) -> codeByte 0x6a <> im
  Push_ (mkImm S32 -> FJust (_, im)) -> codeByte 0x68 <> im
  Push_ dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0a r) mempty
  Push_ dest -> regprefix S32 dest (codeByte 0xff <> reg8 0x6 dest) mempty

  Ret_   -> codeByte 0xc3
  Nop_   -> codeByte 0x90
  PushF_ -> codeByte 0x9c
  PopF_  -> codeByte 0x9d
  Cmc_   -> codeByte 0xf5
  Clc_   -> codeByte 0xf8
  Stc_   -> codeByte 0xf9
  Cli_   -> codeByte 0xfa
  Sti_   -> codeByte 0xfb
  Cld_   -> codeByte 0xfc
  Std_   -> codeByte 0xfd

  J_ (Condition c) (Just S8)  l -> codeByte (0x70 .|. c) <> mkRef S8 1 l
  J_ (Condition c) (Just S32) l -> codeByte 0x0f <> codeByte (0x80 .|. c) <> mkRef S32 4 l
  J_ (Condition c) Nothing  l -> mkAutoRef [(S8, [0x70 .|. c]), (S32, [0x0f, 0x80 .|. c])] l

  Jmp_ (Just S8)  l -> codeByte 0xeb <> mkRef S8 1 l
  Jmp_ (Just S32) l -> codeByte 0xe9 <> mkRef S32 4 l
  Jmp_ Nothing  l -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l

  Jmpq_ (ImmOp (LabelRelValue S32 l)) -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l
  Jmpq_ a -> op1' 0xff 0x4 a

  Label_ -> CodeBuilder 0 0 $ do
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


  Data_ x -> codeBytes x

  Align_ s -> CodeBuilder 0 (s-1) $ do
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

  xchg_a :: IsSize s => Operand r s -> CodeBuilder
  xchg_a dest@(RegOp r) | size dest /= S8 = regprefix (size dest) dest (oneReg 0x12 r) mempty
  xchg_a dest = regprefix'' dest 0x43 (reg8 0x0 dest) mempty

  toCode :: HasBytes a => a -> CodeBuilder
  toCode = codeBytes . toBytes

  sizePrefix_ :: [SReg] -> Size -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
  sizePrefix_ rs s r x c im
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

      prefix40 x = iff (x /= 0) $ prefix40_ x
      prefix40_ x = codeByte $ 0x40 .|. x

      maybePrefix40 = iff (any isRex rs || x /= 0) (prefix40_ x)

      displacement :: Operand r s -> CodeBuilder
      displacement (IPMemOp (Immediate d)) = toCode d
      displacement (IPMemOp (LabelRelValue s@S32 d)) = mkRef s (sizeLen s + fromIntegral (codeBuilderLength im)) d
      displacement (MemOp (Addr b d i)) = mkSIB b i <> dispVal b d
        where
          mkSIB _ (IndexReg s (NormalReg n 0x4)) = error ("sp cannot be used as index: register " ++ show n)
          mkSIB _ (IndexReg s i) = f s $ reg8_ i
          mkSIB Nothing _ = f s1 0x4
          mkSIB (Just (reg8_ -> 0x4)) _ = f s1 0x4
          mkSIB _ _ = mempty

          f (Scale s) i = codeByte $ s `shiftL` 6 .|. i `shiftL` 3 .|. maybe 0x5 reg8_ b

          dispVal Just{} (Disp (Integral (d :: Int8))) = toCode d
          dispVal _ (Disp d) = toCode d
          dispVal Nothing _ = toCode (0 :: Int32)    -- [rbp] --> [rbp + 0]
          dispVal (Just (reg8_ -> 0x5)) _ = codeByte 0    -- [rbp] --> [rbp + 0]
          dispVal _ _ = mempty
      displacement _ = mempty

  reg8_ :: Reg t -> Word8
  reg8_ (NormalReg _ r) = r .&. 0x7
  reg8_ (HighReg _ r) = r .|. 0x4
  reg8_ (XMM _ r) = r .&. 0x7

  regprefix :: IsSize s => Size -> Operand r s -> CodeBuilder -> CodeBuilder -> CodeBuilder
  regprefix s r = sizePrefix_ (regs r) s r (extbits r)

  regprefix2 :: (IsSize s1, IsSize s) => Operand r1 s1 -> Operand r s -> CodeBuilder -> CodeBuilder
  regprefix2 r r' c = sizePrefix_ (regs r <> regs r') (size r) r (extbits r' `shiftL` 2 .|. extbits r) c mempty

  regprefix'' :: IsSize s => Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
  regprefix'' r p c = regprefix (size r) r $ extension r p <> c

  regprefix2' :: (IsSize s1, IsSize s) => Operand r1 s1 -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder
  regprefix2' r r' p c = regprefix2 r r' $ extension r p <> c

  sse :: IsSize s => Word8 -> Operand r S128 -> Operand r' s -> CodeBuilder
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
      operMode (MemOp (Addr (Just (reg8_ -> 0x5)) NoDisp _)) = 0x1   -- [rbp] --> [rbp + 0]
      operMode (MemOp (Addr Nothing _ _)) = 0x0
      operMode (MemOp (Addr _ NoDisp _))  = 0x0
      operMode (MemOp (Addr _ (Disp (Integral (_ :: Int8))) _))  = 0x1
      operMode (MemOp (Addr _ Disp{} _))  = 0x2
      operMode IPMemOp{}          = 0x0
      operMode _              = 0x3

      rc :: Operand r s -> Word8
      rc (MemOp (Addr (Just r) _ NoIndex)) = reg8_ r
      rc MemOp{}   = 0x04    -- SIB byte
      rc IPMemOp{} = 0x05
      rc (RegOp r) = reg8_ r

  op2 :: IsSize s => Word8 -> Operand RW s -> Operand r s -> CodeBuilder
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

  op2' :: IsSize s => Word8 -> Operand RW s -> Operand RW s -> CodeBuilder
  op2' op dest src@RegOp{} = op2g op dest src
  op2' op dest@RegOp{} src = op2g (op .|. 0x1) src dest

  op2g :: (IsSize t, IsSize s) => Word8 -> Operand r s -> Operand r' t -> CodeBuilder
  op2g op dest src = regprefix2' dest src op $ reg2x8 src dest

  reg2x8 :: (IsSize s, IsSize s') => Operand r s -> Operand r' s' -> CodeBuilder
  reg2x8 (RegOp r) = reg8 (reg8_ r)

  op1_ :: IsSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder -> CodeBuilder
  op1_ r1 r2 dest = regprefix'' dest r1 (reg8 r2 dest)

  op1 :: IsSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder
  op1 a b c = op1_ a b c mempty

  op1' :: Word8 -> Word8 -> Operand r S64 -> CodeBuilder
  op1' r1 r2 dest = regprefix S32 dest (codeByte r1 <> reg8 r2 dest) mempty

  shiftOp :: IsSize s => Word8 -> Operand RW s -> Operand r S8 -> CodeBuilder
  shiftOp c dest (ImmOp (Immediate 1)) = op1 0x68 c dest
  shiftOp c dest (mkImmU S8 -> FJust (_, i)) = op1_ 0x60 c dest i
  shiftOp c dest RegCl = op1 0x69 c dest
  shiftOp _ _ _ = error "invalid shift operands"

  oneReg :: Word8 -> Reg t -> CodeBuilder
  oneReg x r = codeByte $ x `shiftL` 3 .|. reg8_ r

showCode = \case
  EmptyCode  -> return ()
  AppendCode _ a b -> showCode a >> showCode b
  Prebuilt _ c -> showCodeLine (Align_ 4) >> codeLine "{" >> showCode c >> codeLine "}"
  CodeLine _ x -> showCodeLine x