{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module JVM.Jasmine where
import Text.RawString.QQ
import Data.List

data JConstant = JNumber Int

data JDirective =
  LimitStack Int
  | LimitLocals Int
  | Line Int

data JIntOp =
  Mul
  | Div
  | Add
  | Sub

data JInstruction =
  Directive JDirective
  | Push JConstant
  | StoreInt Int
  | ConstInt String
  | LoadInt Int
  | IntOp JIntOp
  | InvokeStatic String
  | Return
  | Pop
  | Swap

data JMethod =
  Method Bool Bool

jasminePush :: JConstant -> String
jasminePush (JNumber val)
  | val >= 0 && val <= 255 = "bipush " ++ show val
  | val > 255 && val <= 65535 = "sipush" ++ show (div val 256) ++ " " ++ show (mod val 256)
  | True = "ldc " ++ show val

jasminePushSize :: JConstant -> Int
jasminePushSize (JNumber val)
  | val >= 0 && val <= 255 = 1
  | val > 255 && val <= 65535 = 2
  | True = 2

jasmineStoreInt :: Int -> String
jasmineStoreInt index
  | index >= 0 && index <= 3 = "istore_" ++ show index
  | True = "istore " ++ show index

jasmineConstInt :: String -> String
jasmineConstInt index = "iconst_" ++ index

jasmineLoadInt :: Int -> String
jasmineLoadInt index
  | index >= 0 && index <= 3 = "iload_" ++ show index
  | True = "iload " ++ show index

jasmineOpInt :: JIntOp -> String
jasmineOpInt Div = "idiv"
jasmineOpInt Mul = "imul"
jasmineOpInt Add = "iadd"
jasmineOpInt Sub = "isub"

jasmineDirective :: JDirective -> String
jasmineDirective (LimitStack val) = ".limit stack " ++ show val
jasmineDirective (LimitLocals val) = ".limit locals " ++ show val
jasmineDirective (Line val) = ".line " ++ show val

instructionToJasmine :: JInstruction -> String
instructionToJasmine Pop = "pop"
instructionToJasmine (Directive dir) = jasmineDirective dir
instructionToJasmine (Push val) = jasminePush val
instructionToJasmine Swap = "swap"
instructionToJasmine (StoreInt index) = jasmineStoreInt index
instructionToJasmine (LoadInt index) = jasmineLoadInt index
instructionToJasmine (IntOp op) = jasmineOpInt op
instructionToJasmine (InvokeStatic ins) = "invokestatic " ++ ins
instructionToJasmine Return = "return"
instructionToJasmine (ConstInt v) = jasmineConstInt v

jasmineGeneratePrefix :: Int -> JInstruction -> (String, Int)
jasmineGeneratePrefix i _ = ("", i)

jasmineInstructions :: String -> [JInstruction] -> (String, Int)
jasmineInstructions globalPrefix ins = foldl (\(acc, i) (el, ins) -> let (prefix, newI) = jasmineGeneratePrefix i ins in
  if acc == "" then ((globalPrefix ++ prefix ++ el), newI) else ((acc ++ "\n" ++ globalPrefix ++ prefix ++ el), newI)) ("", 0) $ map (\(_, v) -> ((instructionToJasmine v), v)) $ zip [0..] ins