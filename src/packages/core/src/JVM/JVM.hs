{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module JVM.JVM where
import Text.RawString.QQ

import Compiler.Compiler
import Syntax.Base
import Shelly

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.Array
import qualified Data.Map as M

import JVM.Jasmine
import JVM.Inspection

import JVM.OptimizeStackOrder

generateManifest :: String
generateManifest = [r|Manifest-Version: 1.0
Main-Class: com.instant.Main
|]

runCompilationTools :: String -> IO ()
runCompilationTools content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/jvm/com/instant"]
  _ <- liftIO $ writeFile "./insc_build/jvm/Main.mf" generateManifest
  _ <- liftIO $ writeFile "./insc_build/jvm/main.j" content
  bash "cp" ["-rf", "./lib/Runtime.java", "./insc_build/jvm/com/instant/Runtime.java"]
  bash "java" ["-jar", "./bin/jasmin.jar", "-d", "./insc_build/jvm", "./insc_build/jvm/main.j"]
  bash "javac" ["./insc_build/jvm/com/instant/Runtime.java"]
  cd "./insc_build/jvm"
  bash "jar" ["cmf", "Main.mf", "Main.jar", "./com/instant/Main.class", "./com/instant/Runtime.class"]
  cd "../.."
  bash "cp" ["-rf", "./insc_build/jvm/Main.jar", "."]
  return ()

compileExp :: Exp -> Exec [JInstruction]
compileExp (ExpAdd l r) = do
  cl <- compileExp l
  cr <- compileExp r
  return $ cl ++ cr ++ [IntOp Add]
compileExp (ExpDiv l r) = do
  cl <- compileExp l
  cr <- compileExp r
  return $ cl ++ cr ++ [IntOp Div]
compileExp (ExpMul l r) = do
  cl <- compileExp l
  cr <- compileExp r
  return $ cl ++ cr ++ [IntOp Mul]
compileExp (ExpSub l r) = do
  cl <- compileExp l
  cr <- compileExp r
  return $ cl ++ cr ++ [IntOp Sub]
compileExp (ExpLit val) = do
  return [Push $ JNumber $ fromIntegral val]
compileExp (ExpVar (Ident name)) = do
  env <- ask
  (Just (Local index)) <- return $ getVar name env
  return [LoadInt index]

locationToStoreInst :: Location -> JInstruction
locationToStoreInst (Local index) = StoreInt index

compileStmt :: Stmt -> Exec ([JInstruction], Environment)
compileStmt (SAss (Ident name) exp) = do
  env <- ask
  compiledExp <- compileExp exp
  (loc, env1) <- return $ defineAndAlloc name env
  out <- return $ compiledExp ++ [locationToStoreInst loc]
  return (out, env1)
compileStmt (SExp exp) = do
  env <- ask
  compiledExp <- compileExp exp
  return (compiledExp, env)

compile :: Program -> Exec ([JInstruction], Environment)
compile (Prog statements) = do
  env <- ask
  (pOut, pEnv) <- foldM (\(out, env) ins -> do
    (newOut, newEnv) <- local (\_ -> env) $ compileStmt ins
    return (out ++ newOut, newEnv)) ([], env) statements
  return (pOut ++ [
    InvokeStatic "com/instant/Runtime/printInt(I)V",
    Return], pEnv)

compilerJVM :: Program -> Exec (String, Environment)
compilerJVM program = do
  header <- return $ [r|.bytecode 57.0
     .source Main.java
     .class public com/instant/Main
     .super java/lang/Object

     .method public <init>()V
       .limit stack 1
       .limit locals 1
       .line 4
       0: aload_0
       1: invokespecial java/lang/Object/<init>()V
       4: return
     .end method

     .method public static main([Ljava/lang/String;)V
    |]
  footer <- return $ [r|
       .end method
      |]
  (compiledProgram, env) <- compile $ optimizeStackOrder program
  stackLimit <- return $ getStackSize compiledProgram
  localsLimit <- return $ getLocalsSize compiledProgram
  (insContent, _) <- return $ jasmineInstructions "         " $ [ Directive $ LimitStack stackLimit, Directive $ LimitLocals localsLimit ] ++ compiledProgram
  content <- return $ header ++ insContent ++ footer
  _ <- liftIO $ putStrLn content
  _ <- liftIO $ runCompilationTools content
  return (":)", env)
