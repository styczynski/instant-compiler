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
module X86.Generator.Syntax.Registers where

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

-- | Register name.
data RegName = RegName String | RegUnallocated String | RegNameUnknown deriving (Eq, Ord)

instance Show RegName where
  show (RegName name) = name
  show (RegUnallocated id) = "<unallocated:" ++ show id ++ ">"
  show RegNameUnknown = "<?>"

-- | A register.
data Reg :: Size -> * where
  NormalReg :: RegName -> Word8 -> Reg s      -- \"normal\" registers are for example @AL@, @BX@, @ECX@ or @RSI@
  HighReg   :: RegName -> Word8 -> Reg Size8B     -- \"high\" registers are @AH@, @BH@, @CH@ etc
  XMM       :: RegName -> Word8 -> Reg Size128B   -- XMM registers
deriving instance Eq (Reg s)
deriving instance Ord (Reg s)

class FromReg c where
  fromReg :: Reg s -> c s

type BaseReg s = Maybe (Reg s)

data IndexReg s = NoIndex | IndexReg Scale (Reg s)
  deriving (Eq)

-- | A (relative) address is made up base a base register, a displacement, and a (scaled) index.
-- For example in @[eax+4*ecx+20]@ the base register is @eax@, the displacement is @20@ and the
-- index is @4*ecx@.
data RelativeAddress s = RelativeAddress
  { baseReg        :: BaseReg s
  , displacement   :: Displacement
  , indexReg       :: IndexReg s
  }
  deriving (Eq)

type Displacement = Maybe Int32

data Address :: Size -> * where
  Address :: [(Int, Reg s)] -> Int -> Address s

instance WithTypedSize s => Show (Reg s) where
  show (XMM _ i) = "xmm" ++ show i
  show (HighReg _ i) =
    (["ah", " ch", "dh", "bh"] ++ repeat (error ("show @Reg")))
      !! fromIntegral i

  show r@(NormalReg _ i) =
    (!! fromIntegral i) . (++ repeat (error ("show @Reg"))) $ case size r of
      Size8B ->
        ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil"] ++ map (++ "b") r8
      Size16B -> r0 ++ map (++ "w") r8
      Size32B -> map ('e' :) r0 ++ map (++ "d") r8
      Size64B -> map ('r' :) r0 ++ r8
   where
    r0 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
    r8 = ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]

instance WithTypedSize s => HasSize (BaseReg s) where
  size _ = size (getSizeOf :: TypedSize s)

instance WithTypedSize s => HasSize (Reg s) where
  size _ = size (getSizeOf :: TypedSize s)

instance WithTypedSize s => HasSize (IndexReg s) where
  size _ = size (getSizeOf :: TypedSize s)

instance Semigroup (IndexReg s) where
  i <> NoIndex = i
  NoIndex <> i = i

instance Monoid (IndexReg s) where
  mempty = NoIndex

instance WithTypedSize s => Show (RelativeAddress s) where
  show (RelativeAddress b d i) = showSum $ shb b ++ shd d ++ shi i
   where
    shb Nothing  = []
    shb (Just x) = [(True, show x)]
    shd NoDisp   = []
    shd (Disp x) = [(signum x /= (-1), show (abs x))]
    shi NoIndex         = []
    shi (IndexReg sc x) = [(True, show' (getScaleFactor sc) ++ show x)]
    show' 1 = ""
    show' n = show n ++ " * "
    showSum []                = "0"
    showSum ((True , x) : xs) = x ++ g xs
    showSum ((False, x) : xs) = "-" ++ x ++ g xs
    g = concatMap (\(a, b) -> f a ++ b)
    f True  = " + "
    f False = " - "

instance WithTypedSize s => HasSize (RelativeAddress s) where
  size _ = size (getSizeOf :: TypedSize s)

instance WithTypedSize s => HasSize (Address s) where
  size _ = size (getSizeOf :: TypedSize s)

instance Semigroup (RelativeAddress s) where
  RelativeAddress a b c <> RelativeAddress a' b' c' = RelativeAddress (getFirst $ First a <> First a') (getFirst $ First b <> First b') (c <> c')

instance Monoid (RelativeAddress s) where
  mempty = RelativeAddress (getFirst mempty) (getFirst mempty) mempty


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

pattern NoDisp = Nothing
pattern Disp a = Just a

base :: Reg s -> RelativeAddress s
base x = RelativeAddress (Just x) NoDisp NoIndex

index :: Scale -> Reg s -> RelativeAddress s
index sc x = RelativeAddress Nothing NoDisp (IndexReg sc x)

index' :: Int -> Reg s -> RelativeAddress s
index' sc x = RelativeAddress Nothing NoDisp (IndexReg (toScale sc) x)

index1 = index s1
index2 = index s2
index4 = index s4
index8 = index s8

disp :: (Bits a, Integral a) => a -> RelativeAddress s
disp (Integral x)
  | x == 0 = mempty
  | otherwise = RelativeAddress Nothing (Disp x) NoIndex

scaleAddress :: (Int -> Int) -> Address s -> Address s
scaleAddress f (Address rs d) = Address (first f <$> rs) $ f d

makeRelativeAddress :: Address s -> RelativeAddress s
makeRelativeAddress (Address [(1, r)] d) = base r <> disp d
makeRelativeAddress (Address [(t, r)] d) = index' t r <> disp d
makeRelativeAddress (Address [(1, r), (1, r'@(NormalReg _ 0x4))] d) = base r' <> index1 r <> disp d
makeRelativeAddress (Address [(1, r), (t, r')] d) = base r <> index' t r' <> disp d
makeRelativeAddress (Address [(t, r'), (1, r)] d) = base r <> index' t r' <> disp d

instance FromReg Reg where
  fromReg = id

instance FromReg Address where
  fromReg r = Address [(1, r)] 0

reg a b = fromReg $ NormalReg a b