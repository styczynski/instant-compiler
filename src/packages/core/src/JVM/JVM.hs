{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module JVM.JVM where
import Text.RawString.QQ

import Text.Heterocephalus (compileText)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.ByteString.Lazy.UTF8 (toString)

import Compiler.Compiler
import Syntax.Base
import Shelly

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.Array
import qualified Data.Map as M
import qualified Data.Text as T

import JVM.Jasmine
import JVM.Inspection

import qualified Data.Char as Char

--| Generate Java manifest
generateManifest :: JVMCompilerConfiguration -> String
generateManifest opts = let programClassName = jvmProgramName opts in toString $ renderMarkup [compileText|Manifest-Version: 1.0
Main-Class: #{programClassName}
|]

--| Compiler configuration for JVM
data JVMCompilerConfiguration = JVMCompilerConfiguration {
  jvmLibLocation :: String,
  jvmBinLocation :: String,
  jvmRunProgram :: Bool,
  jvmProgramName :: String,
  jvmOutputPath :: String
}

--| Default compiler configuration
defaultJVMCompilerConfiguration :: JVMCompilerConfiguration
defaultJVMCompilerConfiguration = JVMCompilerConfiguration {
  jvmLibLocation = ".",
  jvmBinLocation = ".",
  jvmRunProgram = False,
  jvmProgramName = "main",
  jvmOutputPath = "."
}

--| Run compiler commands
runCompilationTools :: JVMCompilerConfiguration -> String -> IO ()
runCompilationTools opts content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/jvm"]
  _ <- liftIO $ putStrLn "Generate Manifest file..."
  _ <- liftIO $ writeFile ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".mf") $ generateManifest opts
  _ <- liftIO $ writeFile ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".j") content
  _ <- liftIO $ putStrLn "Prepare runtime boilerplate..."
  bash "cp" ["-rf", T.pack (jvmLibLocation opts ++ "/lib/Runtime.java"), "./insc_build/jvm/Runtime.java"]
  _ <- liftIO $ putStrLn "Compile with Jasmine..."
  bash "java" ["-jar", T.pack (jvmBinLocation opts ++ "/lib/jasmin.jar"), "-d", "./insc_build/jvm", T.pack ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".j")]
  _ <- liftIO $ putStrLn "Compile runtime boilerplate..."
  bash "javac" ["./insc_build/jvm/Runtime.java"]
  cd "./insc_build/jvm"
  _ <- liftIO $ putStrLn "Create executable jar..."
  bash "jar" ["cmf", T.pack (jvmProgramName opts ++ ".mf"), T.pack (jvmProgramName opts ++ ".jar"), T.pack ("./" ++ jvmProgramName opts ++ ".class"), "./Runtime.class"]
  cd "../.."
  _ <- liftIO $ putStrLn "Finalize..."
  bash "cp" ["-rf", T.pack ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".class"), T.pack (jvmOutputPath opts)]
  bash "cp" ["-rf", T.pack ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".j"), T.pack (jvmOutputPath opts)]
  bash "cp" ["-rf", T.pack ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".jar"), T.pack (jvmOutputPath opts)]
  return ()

--| Optimize expressions stack depth by swapping L/R subtree
optimizeExpStackBiAlloc :: Exp -> Exp -> Exec ([JInstruction], [JInstruction], Int, Bool)
optimizeExpStackBiAlloc l r = do
  (ol, dl) <- compileExp l
  (or, dr) <- compileExp r
  return $ if dl <= dr then (ol, or, dr+1, False) else (or, ol, dl+1, True)

--| Conditionally create swap instruction
makeSwap :: Bool -> [JInstruction]
makeSwap False = []
makeSwap True = [Swap]

--| Compile instant expressions
compileExp :: Exp -> Exec ([JInstruction], Int)
compileExp (ExpAdd l r) = do
  (cl, cr, s, _) <- optimizeExpStackBiAlloc l r
  return (cl ++ cr ++ [IntOp Add], s)
compileExp (ExpDiv l r) = do
  (cl, cr, s, swap) <- optimizeExpStackBiAlloc l r
  return (cl ++ cr ++ makeSwap swap ++ [IntOp Div], s)
compileExp (ExpMul l r) = do
  (cl, cr, s, _) <- optimizeExpStackBiAlloc l r
  return (cl ++ cr ++ [IntOp Mul], s)
compileExp (ExpSub l r) = do
  (cl, cr, s, swap) <- optimizeExpStackBiAlloc l r
  return (cl ++ cr ++ makeSwap swap ++ [IntOp Sub], s)
compileExp (ExpLit val) = do
  if val >= -1 && val <= 5 then do return (compileConstPush $ fromIntegral val, 1) else return ([Push $ JNumber $ fromIntegral val], 1)
compileExp (ExpVar (Ident name)) = do
  env <- ask
  (Just (Local index)) <- return $ getVar name env
  return ([LoadInt index], 1)

--| Translate location to store instruction
locationToStoreInst :: Location -> JInstruction
locationToStoreInst (Local index) = StoreInt index

--| Push constants
compileConstPush :: Int -> [JInstruction]
compileConstPush (-1) = [ConstInt "m1"]
compileConstPush v = [ConstInt $ show v]

--| Compile instant statements
compileStmt :: Stmt -> Exec ([JInstruction], Environment)
compileStmt (SAss (Ident name) exp) = do
  env <- ask
  (compiledExp, _) <- compileExp exp
  (loc, env2) <- return $ defineAndAlloc name env
  let out = compiledExp ++ [locationToStoreInst loc]
  return (out, env2)
compileStmt (SExp exp) = do
  env <- ask
  (compiledExp, _) <- compileExp exp
  return (compiledExp ++ [InvokeStatic "Runtime/printInt(I)V"], env)

--| Compile instant program
compile :: Program -> Exec ([JInstruction], Environment)
compile (Prog statements) = do
  statements <- return statements
  env <- ask
  (pOut, pEnv) <- foldM (\(out, env) ins -> do
    (newOut, newEnv) <- local (const env) $ compileStmt ins
    return (out ++ newOut, newEnv)) ([], env) statements
  return (pOut ++ [Return], pEnv)

--| Create a compiler for JVM target with default settings
defaultCompilerJVM :: Program -> Exec (String, Environment)
defaultCompilerJVM = compilerJVM defaultJVMCompilerConfiguration

--| Post-compile hook
postCompile :: JVMCompilerConfiguration -> Exec (String, Environment)
postCompile opts = do
  env <- ask
  out <- if jvmRunProgram opts then shelly $ silently $ bash "java" ["-jar", T.pack (jvmOutputPath opts ++ "/" ++ jvmProgramName opts ++ ".jar")] else return "Post-compile hook finished."
  return (T.unpack out, env)

--| Create compiler targeting JVM
compilerJVM :: JVMCompilerConfiguration -> Program -> Exec (String, Environment)
compilerJVM opts program = do
  _ <- liftIO $ putStrLn "Compile Instant code..."
  let programClassName = jvmProgramName opts
  let header = toString $ renderMarkup [compileText|.bytecode 52.0
     .source #{programClassName}.java
     .class public #{programClassName}
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
  let footer = [r|
     .end method
|]
  (compiledProgram, env) <- compile program
  let stackLimit = getStackSize compiledProgram
  let localsLimit = getLocalsSize compiledProgram
  (insContent, _) <- return $ jasmineInstructions "       " $ [ Directive $ LimitStack stackLimit, Directive $ LimitLocals localsLimit ] ++ compiledProgram
  let content = header ++ insContent ++ footer
  _ <- liftIO $ runCompilationTools opts content
  local (const env) $ postCompile opts
