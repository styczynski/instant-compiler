
{-# LANGUAGE OverloadedStrings #-}
module LLVM.Assembly where
import Text.RawString.QQ
import Data.List

--| Define available LLVM instructions
data LInstruction =
  Add String String String String
  | Sub String String String String
  | Div String String String String
  | Mul String String String String
  | Print String

--| Translate instruction to LLVM code
instructionToLLVM :: LInstruction -> String
instructionToLLVM (Add target typeName a b) = target ++ " = add " ++ typeName ++ " " ++ a ++ ", " ++ b
instructionToLLVM (Sub target typeName a b) = target ++ " = sub " ++ typeName ++ " " ++ a ++ ", " ++ b
instructionToLLVM (Div target typeName a b) = target ++ " = sdiv " ++ typeName ++ " " ++ a ++ ", " ++ b
instructionToLLVM (Mul target typeName a b) = target ++ " = mul " ++ typeName ++ " " ++ a ++ ", " ++ b
instructionToLLVM (Print target) = "call void @printInt(i32 " ++ target ++ ")"

--| Generate prefix for instruction. Default implementation does nothing, but function can be used to specify
--  Instruction prefixes for each line
llvmGeneratePrefix :: Int -> LInstruction -> (String, Int)
llvmGeneratePrefix i _ = ("", i)

--| Translate LLVM instructions
llvmInstructions :: String -> [LInstruction] -> (String, Int)
llvmInstructions globalPrefix ins = foldl (\(acc, i) (el, ins) -> let (prefix, newI) = llvmGeneratePrefix i ins in
  if acc == "" then (globalPrefix ++ prefix ++ el, newI) else (acc ++ "\n" ++ globalPrefix ++ prefix ++ el, newI)) ("", 0) $ zipWith (\ _x v -> (instructionToLLVM v, v)) [0..] ins
