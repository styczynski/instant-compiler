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


runCompilationTools :: String -> IO ()
runCompilationTools content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/llvm"]
  _ <- liftIO $ writeFile "./insc_build/llvm/main.ll" content
  bash "cp" ["-rf", "./lib/runtime.ll", "./insc_build/llvm/runtime.ll"]
  bash "llvm-as" ["-o", "./insc_build/llvm/main.bc", "./insc_build/llvm/main.ll"]
  bash "llvm-as" ["-o", "./insc_build/llvm/runtime.bc", "./insc_build/llvm/runtime.ll"]
  bash "llc" ["-o", "./insc_build/llvm/main.s", "./insc_build/llvm/main.bc"]
  bash "llc" ["-o", "./insc_build/llvm/runtime.s", "./insc_build/llvm/runtime.bc"]
  bash "clang" ["-o", "./insc_build/llvm/main", "./insc_build/llvm/main.s", "./insc_build/llvm/runtime.s"]
  return ()

uniqueNameForExpStack :: Exp -> Exec (String, Environment)
uniqueNameForExpStack (ExpLit value) = do
  env <- ask
  return (show value, env)
uniqueNameForExpStack (ExpVar (Ident name)) = do
  env <- ask
  return ("%" ++ name, env)
uniqueNameForExpStack _ = do
  env <- ask
  (sl, env0) <- return $ uniqueName env
  return ("%" ++ sl, env0)

compileExp :: String -> Exp -> Exec ([LInstruction], Environment)
compileExp stackVarName (ExpAdd l r) = do
  env <- ask
  (sl, env0) <- local (\_ -> env) $ uniqueNameForExpStack l
  (sr, env1) <- local (\_ -> env0) $ uniqueNameForExpStack r
  (cl, env2) <- local (\_ -> env1) $ compileExp sl l
  (cr, env3) <- local (\_ -> env2) $ compileExp sr r
  return (cl ++ cr ++ [Add stackVarName "i32" sl sr], env3)
compileExp stackVarName (ExpSub l r) = do
  env <- ask
  (sl, env0) <- local (\_ -> env) $ uniqueNameForExpStack l
  (sr, env1) <- local (\_ -> env0) $ uniqueNameForExpStack r
  (cl, env2) <- local (\_ -> env1) $ compileExp sl l
  (cr, env3) <- local (\_ -> env2) $ compileExp sr r
  return (cl ++ cr ++ [Sub stackVarName "i32" sl sr], env3)
compileExp stackVarName (ExpDiv l r) = do
  env <- ask
  (sl, env0) <- local (\_ -> env) $ uniqueNameForExpStack l
  (sr, env1) <- local (\_ -> env0) $ uniqueNameForExpStack r
  (cl, env2) <- local (\_ -> env1) $ compileExp sl l
  (cr, env3) <- local (\_ -> env2) $ compileExp sr r
  return (cl ++ cr ++ [Div stackVarName "i32" sl sr], env3)
compileExp stackVarName (ExpMul l r) = do
  env <- ask
  (sl, env0) <- local (\_ -> env) $ uniqueNameForExpStack l
  (sr, env1) <- local (\_ -> env0) $ uniqueNameForExpStack r
  (cl, env2) <- local (\_ -> env1) $ compileExp sl l
  (cr, env3) <- local (\_ -> env2) $ compileExp sr r
  return (cl ++ cr ++ [Mul stackVarName "i32" sl sr], env3)
compileExp stackVarName (ExpLit _) = do
  env <- ask
  return ([], env)
compileExp stackVarName (ExpVar _) = do
  env <- ask
  return ([], env)

compileStmt :: Stmt -> Exec ([LInstruction], Environment)
compileStmt (SAss (Ident name) (ExpLit val)) = do
  env <- ask
  return ([Add ("%" ++ name) "i32" (show val) "0"], env)
compileStmt (SAss (Ident name) (ExpVar (Ident aName))) = do
  env <- ask
  return ([Add ("%" ++ name) "i32" ("%" ++ aName) "0"], env)
compileStmt (SAss (Ident name) exp) = compileExp ("%" ++ name) exp

translateStmtI :: (Stmt, Int) -> Int -> Exec ([Stmt], Environment)
translateStmtI ((SExp exp), index) len = do
  env <- ask
  (tmp, tmpEnv) <- return $ uniqueName env
  return ([SAss (Ident tmp) exp], tmpEnv)
translateStmtI (stmt, _) _ = do
  env <- ask
  return ([stmt], env)

translateStmtEndI :: (Stmt, Int) -> Int -> Exec ([Stmt], Environment)
translateStmtEndI ((SExp exp), index) len = do
  env <- ask
  return ([SAss (Ident "result") exp], env)
translateStmtEndI (stmt@(SAss (Ident name) _), _) _ =  do
  env <- ask
  return ([stmt, SAss (Ident "result") (ExpVar $ Ident name)], env)

translateStmt :: [Stmt] -> Exec ([Stmt], Environment)
translateStmt statements = do
  len <- return $ length statements
  env <- ask
  foldM (\(acc, env) (index, e) -> do
    (s, e) <- if index == len - 1 then local (\_ -> env) $ translateStmtEndI (e, index) len else local (\_ -> env) $ translateStmtI (e, index) len
    return (acc ++ s, e)) ([], env) $ zip [0..] statements

compile :: Program -> Exec ([LInstruction], Environment)
compile (Prog statements) = do
  env <- ask
  (statements, env) <- translateStmt statements
  (pOut, pEnv) <- foldM (\(out, env) ins -> do
      (newOut, newEnv) <- local (\_ -> env) $ compileStmt ins
      return (out ++ newOut, newEnv)) ([], env) $ statements
  return (pOut, pEnv)

compilerLLVM :: Program -> Exec (String, Environment)
compilerLLVM program = do
  header <- return $ [r|declare void @printInt(i32)
       define i32 @main() {
|]
  footer <- return $ [r|
       call void @printInt(i32 %result)
       ret i32 0
   }
    |]
  (compiledProgram, env) <- compile program
  (insContent, _) <- return $ llvmInstructions "       " compiledProgram
  content <- return $ header ++ insContent ++ footer
  _ <- liftIO $ putStrLn content
  _ <- liftIO $ runCompilationTools content
  return ("OK", env)
