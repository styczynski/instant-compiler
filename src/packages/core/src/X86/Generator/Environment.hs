{-# language GeneralizedNewtypeDeriving #-}
{-# language NoMonomorphismRestriction #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
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
module X86.Generator.Environment where

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
import X86.Generator.Registers

tellAddr = CodeBuilder 0 0 $ do
  (c, _, _) <- lift getPast
  tell [Left c]

class (Monoid a, Show a) => BuilderCode a l | a -> l where
  codePrebuilt :: V.Vector Word8 -> a -> a
  emptyCodeInstr :: a
  emptyCodeLine :: l
  appendCodeInstr :: CodeBuilder -> a -> a -> a
  codeLineInstr :: CodeBuilder -> l -> a
  createCodeBuilder :: a -> CodeBuilder
  createCodeBuilder' :: a -> l -> CodeBuilder
  newCodeLine :: a -> l -> Code a l
  newCodeLine a l = CodeM $ tell $ codeLineInstr (tellAddr <> createCodeBuilder' a l) l
  createCodeLine :: l -> Code a l
  createCodeLine = newCodeLine emptyCodeInstr

type CodeBuilderRes = [Either Int (Int, Word8)]

type CodeBuilderTardis = Tardis (Int, [Int]) (Int, [Int], LabelState)

data CodeBuilder = CodeBuilder
  { minLen, maxLen :: Int
  , getCodeBuilder :: WriterT CodeBuilderRes CodeBuilderTardis ()
  }

type LabelState = [[(Size, Int, Int)]]

data CodeState = CodeState {
  labelPtr :: Int,
  regPtr :: Int,
  funcMap :: Map.Map String Label
} deriving (Show)

newtype (BuilderCode c l) => CodeM c l a = CodeM {unCodeM :: StateT CodeState (Writer c) a}
  deriving (Functor, Applicative, Monad, MonadFix)

emptyCodeState :: CodeState
emptyCodeState = CodeState {
  labelPtr = 0,
  regPtr = 0,
  funcMap = Map.empty
}

type Code a l = CodeM a l ()

takes [] _ = []
takes (i: is) xs = take i xs: takes is (drop i xs)

iff b a = if b then a else mempty

indicator :: Integral a => Bool -> a
indicator False = 0x0
indicator True  = 0x1

pattern FJust a = First (Just a)
pattern FNothing = First Nothing

integralToBytes :: (Bits a, Integral a) => Bool{-signed-} -> Size -> a -> Maybe Bytes
integralToBytes False S64 w = toBytes <$> (toIntegralSized w :: Maybe Word64)
integralToBytes False S32 w = toBytes <$> (toIntegralSized w :: Maybe Word32)
integralToBytes False S16 w = toBytes <$> (toIntegralSized w :: Maybe Word16)
integralToBytes False S8  w = toBytes <$> (toIntegralSized w :: Maybe Word8)
integralToBytes True  S64 w = toBytes <$> (toIntegralSized w :: Maybe Int64)
integralToBytes True  S32 w = toBytes <$> (toIntegralSized w :: Maybe Int32)
integralToBytes True  S16 w = toBytes <$> (toIntegralSized w :: Maybe Int16)
integralToBytes True  S8  w = toBytes <$> (toIntegralSized w :: Maybe Int8)

-- multi-byte nop operations
nops :: Int -> Bytes
nops = \case
  0 -> []
  1 -> [0x90]
  2 -> [0x66, 0x90]
  3 -> [0x0f, 0x1f, 0x00]
  4 -> [0x0f, 0x1f, 0x40, 0x00]
  5 -> [0x0f, 0x1f, 0x44, 0x00, 0x00]
  6 -> [0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00]
  7 -> [0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00]
  8 -> [0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]
  9 -> [0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]
  ((+(-2)) -> Integral x) -> [0xeb] ++ toBytes (x :: Int8) ++ replicate (fromIntegral x) 0x00
  ((+(-5)) -> Integral x) -> [0xe9] ++ toBytes (x :: Int32) ++ replicate (fromIntegral x) 0x00

pattern OpXMM <- RegOp XMM{}

codeBuilderLength (CodeBuilder a b _) | a == b = a

instance Semigroup CodeBuilder where
  CodeBuilder mi ma a <> CodeBuilder mi' ma' b = CodeBuilder (min mi mi') (max ma ma') $ a >> b

instance Monoid CodeBuilder where
  mempty = CodeBuilder 0 0 $ return ()

codeBytes :: [Word8] -> CodeBuilder
codeBytes bs = CodeBuilder n n $ do
  c <- lift $ mdo
    (c, ls, ps) <- getPast
    sendFuture (c + n, ls, ps)
    sendPast (ma + n, mls)
    ~(ma, mls) <- getFuture
    return c
  tell $ Right <$> zip [c..] bs
  where
  n = length bs

codeByte :: Word8 -> CodeBuilder
codeByte = codeBytes . (:[])

mkRef :: Size -> Int -> Label -> CodeBuilder
mkRef s@(sizeLen -> sn) offset (Label l_) = CodeBuilder sn sn $ do
  bs <- lift $ mdo
    (n, ls, ps) <- getPast
    sendFuture (n + sn, ls, ps')
    sendPast (ma + sn, mls)
    ~(ma, mls) <- getFuture
    let i = ls !! (- l - 1)
        vx = i - n - offset
        z = case s of
          S8  -> case vx of
            Integral j -> toBytes (j :: Int8)
            _ -> error $ show vx ++ " does not fit into an Int8"
          S32  -> case vx of
            Integral j -> toBytes (j :: Int32)
            _ -> error $ show vx ++ " does not fit into an Int32"
        ~(bs, ps')
          | l < 0 = (z, ps)
          | otherwise = ([], ins l (s, n, - n - offset) ps)
        l = l_ - length ls
    return $ zip [n..] bs
  tell $ Right <$> bs

ins :: Int -> a -> [[a]] -> [[a]]
ins 0 a [] = [[a]]
ins 0 a (as:ass) = (a:as): ass
ins n a [] = []: ins (n-1) a []
ins n a (as: ass) = as: ins (n-1) a ass

mkAutoRef :: [(Size, Bytes)] -> Label -> CodeBuilder
mkAutoRef ss (Label l_) = CodeBuilder (minimum sizes) (maximum sizes) $ do
  bs <- lift $ mdo
    (n, ls, ps) <- getPast
    sendFuture (n + sn, ls, ps')
    sendPast (ma + maximum sizes, mls)
    ~(ma, mls) <- getFuture
    let i = ls !! (- l - 1)
        vx = i - n
        z = g ss

        g [] = error $ show vx ++ " does not fit into auto size"
        g ((s, c): ss) = case (s, vx - length c - sizeLen s) of
          (S8,  Integral j) -> c <> toBytes (j :: Int8)
          (S32, Integral j) -> c <> toBytes (j :: Int32)
          _ -> g ss

        ~(sn, bs, ps')
          | l < 0 = (length z, z, ps)
          | otherwise = (nz, z', ins l (s, n + length z', - n - nz) ps)

        nz = length z' + sizeLen s
        ma' = mls !! l
        vx' = ma - ma'
        (z', s) = g' ss

        g' [] = error $ show vx' ++ " does not fit into auto size"
        g' ((s, c): ss) = case (s, vx') of
          (S8,  Integral (j :: Int8)) -> (c, s)
          (S32, Integral (j :: Int32)) -> (c, s)
          _ -> g' ss

        l = l_ - length ls
    return $ zip [n..] bs
  tell $ Right <$> bs
  where
  sizes = map (\(s, c) -> sizeLen s + length c) ss

-- prebuild code
preBuild :: (BuilderCode c l) => Code c l -> Code c l
preBuild c = CodeM $ tell $ codePrebuilt (compactCode (buildCode lc)) lc
  where
  lc = withLabels c

------------------------------------------------------- code to code builder

instance (BuilderCode c l) => Show (Code c l) where
  show = show . withLabels

compactCode :: (CodeBuilderRes, Int) -> V.Vector Word8
compactCode (x, s) = V.replicate s 0 V.// [p | Right p <- x]

buildTheCode :: (BuilderCode c l) => Code c l -> (CodeBuilderRes, Int)
buildTheCode = buildCode . withLabels

buildCode :: (BuilderCode c l) => c -> (CodeBuilderRes, Int)
buildCode x = (r, len)
  where
  ((_, r), (_, (len, _, _))) = flip runTardis ((0, []), (0, [], [])) . runWriterT . getCodeBuilder . createCodeBuilder $ x

withLabels :: (BuilderCode c l) => Code c l -> c
withLabels =
  snd . runWriter . flip evalStateT emptyCodeState . unCodeM

