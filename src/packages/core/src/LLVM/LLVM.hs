{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.LLVM where
import Text.RawString.QQ

import Compiler.Compiler
import Syntax.Base
import LLVM.Assembly

import Shelly

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.Array
import qualified Data.Map as M
import qualified Data.Text as T

import Inference.Syntax
import Analyzer.Analyzer
import Lib

instance Analyzable Program String where
  emptyPayload = ""

instance Compilable Program String where
  parse (_, t0) source = let ts = myLexer source in case pProgram ts of
    Bad e -> Left $ FailedParse $ show source
    Ok r -> Right (r, t0)


data LLVMCompilerConfiguration = LLVMCompilerConfiguration {
  llvmLibLocation :: String,
  llvmRunProgram :: Bool,
  llvmProgramName :: String,
  llvmOutputPath :: String
}

defaultLLVMCompilerConfiguration :: LLVMCompilerConfiguration
defaultLLVMCompilerConfiguration = LLVMCompilerConfiguration {
  llvmLibLocation = ".",
  llvmRunProgram = False,
  llvmProgramName = "main",
  llvmOutputPath = "."
}

runCompilationTools :: LLVMCompilerConfiguration -> String -> IO ()
runCompilationTools opts content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/llvm"]
  _ <- liftIO $ writeFile ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".ll") content
  _ <- liftIO $ putStrLn "Prepare runtime boilerplate..."
  bash "cp" ["-rf", T.pack $ (llvmLibLocation opts) ++ "/lib/runtime.ll", "./insc_build/llvm/runtime.ll"]
  _ <- liftIO $ putStrLn "Assembly LLVM code..."
  bash "llvm-as" ["-o", T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".bc"), T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".ll")]
  bash "llvm-as" ["-o", "./insc_build/llvm/runtime.bc", "./insc_build/llvm/runtime.ll"]
  _ <- liftIO $ putStrLn "Compile LLVM code..."
  bash "llc" ["-o", T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".s"), T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".bc")]
  bash "llc" ["-o", "./insc_build/llvm/runtime.s", "./insc_build/llvm/runtime.bc"]
  bash "clang" ["-o", T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts)), T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".s"), "./insc_build/llvm/runtime.s"]
  _ <- liftIO $ putStrLn "Finalize..."
  bash "cp" ["-rf", T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".ll"), T.pack (llvmOutputPath opts)]
  bash "cp" ["-rf", T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts) ++ ".bc"), T.pack (llvmOutputPath opts)]
  return ()

postCompile :: LLVMCompilerConfiguration -> Exec (String, Environment)
postCompile opts = do
  env <- ask
  out <- if (llvmRunProgram opts) then shelly $ silently $ bash (fromText $ T.pack ("./insc_build/llvm/" ++ (llvmProgramName opts))) [] else return "Post-compile hook finished."
  return (T.unpack out, env)

getVarName :: String -> Exec String
getVarName name = do
  env <- ask
  v <- return $ getVarFromScope name env
  case v of
    Nothing -> return name
    (Just id) -> do
      (Just (Local index)) <- return $ getVarLocByID id env
      return $ getUniqueNameFrom name index

generateAssignVarName :: Bool -> String -> Exec (String, Environment)
generateAssignVarName True name = do
  env <- ask
  (newNameIndex, env) <- return $ uniqueNameIndex env
  (newID, env) <- return $ define name env
  (_, env) <- return $ allocateAt newID newNameIndex env
  return $ (getUniqueNameFrom name newNameIndex, env)
generateAssignVarName False name = do
  env <- ask
  return (name, env)

defaultCompilerLLVM :: Compiler Program String
defaultCompilerLLVM p = compilerLLVM defaultLLVMCompilerConfiguration p

compile :: Program -> Exec ([LInstruction], Environment)
compile (Program statements) = do
  env <- ask
  return ([], env)

compilerLLVM :: LLVMCompilerConfiguration -> Compiler Program String
compilerLLVM opts (program@(Program statements), _) = do
  _ <- liftIO $ putStrLn "Compile Instant code..."
  header <- return $ [r|declare void @printInt(i32)
       define i32 @main() {
|]
  footer <- return $ [r|
       ret i32 0
   }
    |]
  (compiledProgram, env) <- compile program
  (insContent, _) <- return $ llvmInstructions "       " compiledProgram
  content <- return $ header ++ insContent ++ footer
  _ <- liftIO $ runCompilationTools opts content
  local (\_ -> env) $ postCompile opts
