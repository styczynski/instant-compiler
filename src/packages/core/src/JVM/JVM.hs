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

import JVM.OptimizeStackOrder

import qualified Data.Char as Char

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

generateManifest :: JVMCompilerConfiguration -> String
generateManifest opts = let programClassName = capitalized $ jvmProgramName opts in toString $ renderMarkup $ [compileText|Manifest-Version: 1.0
Main-Class: com.instant.#{programClassName}
|]

data JVMCompilerConfiguration = JVMCompilerConfiguration {
  jvmLibLocation :: String,
  jvmBinLocation :: String,
  jvmRunProgram :: Bool,
  jvmProgramName :: String
}

defaultJVMCompilerConfiguration :: JVMCompilerConfiguration
defaultJVMCompilerConfiguration = JVMCompilerConfiguration {
  jvmLibLocation = ".",
  jvmBinLocation = ".",
  jvmRunProgram = False,
  jvmProgramName = "main"
}

runCompilationTools :: JVMCompilerConfiguration -> String -> IO ()
runCompilationTools opts content = shelly $ silently $ do
  bash "mkdir" ["-p", "./insc_build/jvm/com/instant"]
  _ <- liftIO $ writeFile ("./insc_build/jvm/" ++ (capitalized $ jvmProgramName opts) ++ ".mf") $ generateManifest opts
  _ <- liftIO $ writeFile ("./insc_build/jvm/" ++ jvmProgramName opts ++ ".j") content
  bash "cp" ["-rf", T.pack ((jvmLibLocation opts) ++ "/lib/Runtime.java"), "./insc_build/jvm/com/instant/Runtime.java"]
  bash "java" ["-jar", T.pack ((jvmBinLocation opts) ++ "/bin/jasmin.jar"), "-d", "./insc_build/jvm", T.pack ("./insc_build/jvm/" ++ (jvmProgramName opts) ++ ".j")]
  bash "javac" ["./insc_build/jvm/com/instant/Runtime.java"]
  cd "./insc_build/jvm"
  bash "jar" ["cmf", T.pack ((capitalized $ jvmProgramName opts) ++ ".mf"), T.pack ((capitalized $ jvmProgramName opts) ++ ".jar"), T.pack ("./com/instant/" ++ (capitalized $ jvmProgramName opts) ++ ".class"), "./com/instant/Runtime.class"]
  cd "../.."
  bash "cp" ["-rf", T.pack ("./insc_build/jvm/com/instant/" ++ (capitalized $ jvmProgramName opts) ++ ".class"), T.pack ("./" ++ (jvmProgramName opts) ++ ".class")]
  bash "cp" ["-rf", T.pack ("./insc_build/jvm/" ++ (capitalized $ jvmProgramName opts) ++ ".jar"), "."]
  return ()

optimizeExpStackBiAlloc :: Exp -> Exp -> Exec ([JInstruction], [JInstruction], Int, Bool)
optimizeExpStackBiAlloc l r = do
  (ol, dl) <- compileExp l
  (or, dr) <- compileExp r
  return $ if dl <= dr then (ol, or, dr+1, True) else (or, ol, dl+1, False)

makeSwap :: Bool -> [JInstruction]
makeSwap False = []
makeSwap True = [Swap]

compileExp :: Exp -> Exec ([JInstruction], Int)
compileExp (ExpAdd l r) = do
  (cl, cr, s, _) <- optimizeExpStackBiAlloc l r
  return $ (cl ++ cr ++ [IntOp Add], s)
compileExp (ExpDiv l r) = do
  (cl, cr, s, swap) <- optimizeExpStackBiAlloc l r
  return $ (cl ++ cr ++ (makeSwap swap) ++ [IntOp Div], s)
compileExp (ExpMul l r) = do
  (cl, cr, s, _) <- optimizeExpStackBiAlloc l r
  return $ (cl ++ cr ++ [IntOp Mul], s)
compileExp (ExpSub l r) = do
  (cl, cr, s, swap) <- optimizeExpStackBiAlloc l r
  return $ (cl ++ cr ++ (makeSwap swap) ++ [IntOp Sub], s)
compileExp (ExpLit val) = do
  if (val >= -1 && val <= 5) then do return (compileConstPush $ fromIntegral val, 1) else return ([Push $ JNumber $ fromIntegral val], 1)
compileExp (ExpVar (Ident name)) = do
  env <- ask
  (Just (Local index)) <- return $ getVar name env
  return ([LoadInt index], 1)

locationToStoreInst :: Location -> JInstruction
locationToStoreInst (Local index) = StoreInt index

compileConstPush :: Int -> [JInstruction]
compileConstPush (-1) = [ConstInt "m1"]
compileConstPush v = [ConstInt $ show v]

compileStmt :: Stmt -> Exec ([JInstruction], Environment)
compileStmt (SAss (Ident name) exp) = do
  env <- ask
  (compiledExp, _) <- compileExp exp
  (loc, env1) <- return $ defineAndAlloc name env
  out <- return $ compiledExp ++ [locationToStoreInst loc]
  return (out, env1)
compileStmt (SExp exp) = do
  env <- ask
  (compiledExp, _) <- compileExp exp
  return (compiledExp ++ [InvokeStatic "com/instant/Runtime/printInt(I)V"], env)

compile :: Program -> Exec ([JInstruction], Environment)
compile (Prog statements) = do
  statements <- return $ statements
  env <- ask
  (pOut, pEnv) <- foldM (\(out, env) ins -> do
    (newOut, newEnv) <- local (\_ -> env) $ compileStmt ins
    return (out ++ newOut, newEnv)) ([], env) statements
  return (pOut ++ [Return], pEnv)

defaultCompilerJVM :: Program -> Exec (String, Environment)
defaultCompilerJVM p = compilerJVM defaultJVMCompilerConfiguration p

postCompile :: JVMCompilerConfiguration -> Exec (String, Environment)
postCompile opts = do
  env <- ask
  out <- if (jvmRunProgram opts) then shelly $ silently $ bash "java" ["-jar", T.pack ("./" ++ (capitalized $ jvmProgramName opts) ++ ".jar")] else return ":)"
  return (T.unpack out, env)

compilerJVM :: JVMCompilerConfiguration -> Program -> Exec (String, Environment)
compilerJVM opts program = do
  programClassName <- return $ capitalized $ jvmProgramName opts
  header <- return $ toString $ renderMarkup $ [compileText|.bytecode 52.0
     .source #{programClassName}.java
     .class public com/instant/#{programClassName}
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
  (insContent, _) <- return $ jasmineInstructions "       " $ [ Directive $ LimitStack stackLimit, Directive $ LimitLocals localsLimit ] ++ compiledProgram
  content <- return $ header ++ insContent ++ footer
  _ <- liftIO $ putStrLn content
  _ <- liftIO $ runCompilationTools opts content
  local (\_ -> env) $ postCompile opts
