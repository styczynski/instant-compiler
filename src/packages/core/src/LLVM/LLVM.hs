{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
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

--| Configuration for the LLVM compiler
data LLVMCompilerConfiguration = LLVMCompilerConfiguration {
  llvmLibLocation :: String,
  llvmRunProgram :: Bool,
  llvmProgramName :: String,
  llvmOutputPath :: String
}

--| Default LLVM compiler configuration
defaultLLVMCompilerConfiguration :: LLVMCompilerConfiguration
defaultLLVMCompilerConfiguration = LLVMCompilerConfiguration {
  llvmLibLocation = ".",
  llvmRunProgram = False,
  llvmProgramName = "main",
  llvmOutputPath = "."
}

--| Run compilation commands
runCompilationTools :: LLVMCompilerConfiguration -> String -> IO ()
runCompilationTools opts content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/llvm"]
  _ <- liftIO $ writeFile ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".ll") content
  _ <- liftIO $ putStrLn "Prepare runtime boilerplate..."
  bash "cp" ["-rf", T.pack $ llvmLibLocation opts ++ "/lib/runtime.ll", "./insc_build/llvm/runtime.ll"]
  _ <- liftIO $ putStrLn "Assembly LLVM code..."
  bash "llvm-as" ["-o", T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".bc"), T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".ll")]
  bash "llvm-as" ["-o", "./insc_build/llvm/runtime.bc", "./insc_build/llvm/runtime.ll"]
  _ <- liftIO $ putStrLn "Compile LLVM code..."
  bash "llc" ["-o", T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".s"), T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".bc")]
  bash "llc" ["-o", "./insc_build/llvm/runtime.s", "./insc_build/llvm/runtime.bc"]
  bash "clang" ["-o", T.pack ("./insc_build/llvm/" ++ llvmProgramName opts), T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".s"), "./insc_build/llvm/runtime.s"]
  _ <- liftIO $ putStrLn "Finalize..."
  bash "cp" ["-rf", T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".ll"), T.pack (llvmOutputPath opts)]
  bash "cp" ["-rf", T.pack ("./insc_build/llvm/" ++ llvmProgramName opts ++ ".bc"), T.pack (llvmOutputPath opts)]
  return ()

--| Post-compile hook
postCompile :: LLVMCompilerConfiguration -> Exec (String, Environment)
postCompile opts = do
  env <- ask
  out <- if llvmRunProgram opts then shelly $ silently $ bash (fromText $ T.pack ("./insc_build/llvm/" ++ llvmProgramName opts)) [] else return "Post-compile hook finished."
  return (T.unpack out, env)

--| Create unique registry name for the given expression
uniqueNameForExpStack :: Exp -> Exec (String, Environment)
uniqueNameForExpStack (ExpLit value) = do
  env <- ask
  return (show value, env)
uniqueNameForExpStack (ExpVar (Ident name)) = do
  env <- ask
  name <- getVarName name
  return ("%" ++ name, env)
uniqueNameForExpStack _ = do
  env <- ask
  (sl, env0) <- return $ uniqueName env
  return ("%" ++ sl, env0)

--| Helper function to evaluate arguments of the binary operation
compileBinaryOpArgs :: Exp -> Exp -> Exec (String, String, [LInstruction], [LInstruction], Environment)
compileBinaryOpArgs l r = do
  env <- ask
  (sl, env0) <- local (const env) $ uniqueNameForExpStack l
  (sr, env1) <- local (const env0) $ uniqueNameForExpStack r
  (cl, env2) <- local (const env1) $ compileExp sl l
  (cr, env3) <- local (const env2) $ compileExp sr r
  return (sl, sr, cl, cr, env3)

--| Compile instant expressions
compileExp :: String -> Exp -> Exec ([LInstruction], Environment)
compileExp stackVarName (ExpAdd l r) = do
  (sl, sr, cl, cr, env) <- compileBinaryOpArgs l r
  return (cl ++ cr ++ [Add stackVarName "i32" sl sr], env)
compileExp stackVarName (ExpSub l r) = do
  (sl, sr, cl, cr, env) <- compileBinaryOpArgs l r
  return (cl ++ cr ++ [Sub stackVarName "i32" sl sr], env)
compileExp stackVarName (ExpDiv l r) = do
  (sl, sr, cl, cr, env) <- compileBinaryOpArgs l r
  return (cl ++ cr ++ [Div stackVarName "i32" sl sr], env)
compileExp stackVarName (ExpMul l r) = do
  (sl, sr, cl, cr, env) <- compileBinaryOpArgs l r
  return (cl ++ cr ++ [Mul stackVarName "i32" sl sr], env)
compileExp stackVarName (ExpLit _) = do
  env <- ask
  return ([], env)
compileExp stackVarName (ExpVar _) = do
  env <- ask
  return ([], env)

--| Get unique variable name from it's name in code
getVarName :: String -> Exec String
getVarName name = do
  env <- ask
  let v = getVarFromScope name env
  case v of
    Nothing -> return name
    (Just id) -> do
      (Just (Local index)) <- return $ getVarLocByID id env
      return $ getUniqueNameFrom name index

--| Generate unique variable name from it's name in code; conditionally behaves as identity
generateAssignVarName :: Bool -> String -> Exec (String, Environment)
generateAssignVarName True name = do
  env <- ask
  (newNameIndex, env) <- return $ uniqueNameIndex env
  (newID, env) <- return $ define name env
  (_, env) <- return $ allocateAt newID newNameIndex env
  return (getUniqueNameFrom name newNameIndex, env)
generateAssignVarName False name = do
  env <- ask
  return (name, env)

--| Compile instant statements
compileStmt :: Bool -> Stmt -> Exec ([LInstruction], Environment)
compileStmt shouldBeUnique (SAss (Ident name) (ExpLit val)) = do
  env <- ask
  (assName, env) <- generateAssignVarName shouldBeUnique name
  return ([Add ("%" ++ assName) "i32" (show val) "0"], env)
compileStmt shouldBeUnique (SAss (Ident name) (ExpVar (Ident aName))) = do
  env <- ask
  aName <- getVarName aName
  (assName, env) <- generateAssignVarName shouldBeUnique name
  return ([Add ("%" ++ assName) "i32" ("%" ++ aName) "0"], env)
compileStmt shouldBeUnique (SAss (Ident name) exp) = do
  oldEnv <- ask
  (assName, env) <- generateAssignVarName shouldBeUnique name
  let envScoped = env { scope = scope oldEnv }
  (retCode, retEnv) <- local (const envScoped) $ compileExp ("%" ++ assName) exp
  return (retCode, retEnv { scope = M.union (scope env) (scope retEnv) })
compileStmt _ (SExp exp) = do
   env <- ask
   (tmp, tmpEnv) <- return $ uniqueName env
   (assIns, env) <- local (const tmpEnv) $ compileStmt False $ SAss (Ident tmp) exp
   return (assIns ++ [Print $ "%" ++ tmp], env)

--| Create a compiler targeting LLVM with the default settings
defaultCompilerLLVM :: Program -> Exec (String, Environment)
defaultCompilerLLVM = compilerLLVM defaultLLVMCompilerConfiguration

--| Compile instant program
compile :: Program -> Exec ([LInstruction], Environment)
compile (Prog statements) = do
  env <- ask
  (pOut, pEnv) <- foldM (\(out, env) ins -> do
      (newOut, newEnv) <- local (const env) $ compileStmt True ins
      return (out ++ newOut, newEnv)) ([], env) statements
  return (pOut, pEnv)

--| Create a compiler targeting LLVM
compilerLLVM :: LLVMCompilerConfiguration -> Program -> Exec (String, Environment)
compilerLLVM opts program@(Prog statements) = do
  _ <- liftIO $ putStrLn "Compile Instant code..."
  let header = [r|declare void @printInt(i32)
       define i32 @main() {
|]
  let footer = [r|
       ret i32 0
   }
    |]
  (compiledProgram, env) <- compile program
  (insContent, _) <- return $ llvmInstructions "       " compiledProgram
  let content = header ++ insContent ++ footer
  _ <- liftIO $ runCompilationTools opts content
  local (const env) $ postCompile opts
