
{-# LANGUAGE OverloadedStrings #-}
module JVM.Jasmine where
import Text.RawString.QQ
import Data.List

--| Jasmine constant
newtype JConstant = JNumber Int

--| Jasmine directives
data JDirective =
  --| Request stack to be at maximum of the given size
  LimitStack Int
  --| Limit locals maximum count
  | LimitLocals Int
  --| Used to specify the line number
  | Line Int

--| Jasmine integer operations
data JIntOp =
  Mul
  | Div
  | Add
  | Sub

--| Jasmine instruction definitions
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

--| Jasmine method definition
data JMethod =
  Method Bool Bool

--| Create push instruction for a given constant
jasminePush :: JConstant -> String
jasminePush (JNumber val)
  | val >= -128 && val <= 127 = "bipush " ++ show val
  | val > -32768 && val <= 32767 = "sipush " ++ show val
  | otherwise = "ldc " ++ show val

--| Determine push instruction size for a given constant
jasminePushSize :: JConstant -> Int
jasminePushSize (JNumber val)
  | val >= 0 && val <= 255 = 1
  | val > 255 && val <= 65535 = 2
  | otherwise = 2

--| Store integer value
jasmineStoreInt :: Int -> String
jasmineStoreInt index
  | index >= 0 && index <= 3 = "istore_" ++ show index
  | otherwise = "istore " ++ show index

--| Push constant integer
jasmineConstInt :: String -> String
jasmineConstInt index = "iconst_" ++ index

--| Load integer
jasmineLoadInt :: Int -> String
jasmineLoadInt index
  | index >= 0 && index <= 3 = "iload_" ++ show index
  | otherwise = "iload " ++ show index

--| Translate Jasmine integer arithmetic operations
jasmineOpInt :: JIntOp -> String
jasmineOpInt Div = "idiv"
jasmineOpInt Mul = "imul"
jasmineOpInt Add = "iadd"
jasmineOpInt Sub = "isub"

--| Translate Jasmine directives
jasmineDirective :: JDirective -> String
jasmineDirective (LimitStack val) = ".limit stack " ++ show val
jasmineDirective (LimitLocals val) = ".limit locals " ++ show val
jasmineDirective (Line val) = ".line " ++ show val

--| Translate Jasmine instructions
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

--| Generate prefix for instruction. Default implementation does nothing, but function can be used to specify
--  Instruction prefixes for each line
jasmineGeneratePrefix :: Int -> JInstruction -> (String, Int)
jasmineGeneratePrefix i _ = ("", i)

--| Translate Jasmine instructions
jasmineInstructions :: String -> [JInstruction] -> (String, Int)
jasmineInstructions globalPrefix ins = foldl (\(acc, i) (el, ins) -> let (prefix, newI) = jasmineGeneratePrefix i ins in
  if acc == "" then (globalPrefix ++ prefix ++ el, newI) else (acc ++ "\n" ++ globalPrefix ++ prefix ++ el, newI)) ("", 0) $ zipWith (\ _x v -> (instructionToJasmine v, v)) [0..] ins
