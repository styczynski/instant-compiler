module Lib where

import Compiler.Compiler
import Syntax.Base

import JVM.JVM

-- Verbosity level
type Verbosity = Int

runInit :: Environment -> IO Environment
runInit = return

runInitEmpty :: IO Environment
runInitEmpty = runInit emptyEnv

-- Run compiler in the given environment
runWith :: Compiler -> Verbosity -> String -> Environment -> IO ExecutionResult
runWith compiler v s env = do
    runCWith compiler v s env

-- Run compiler in the given environment
runCWith :: Compiler -> Verbosity -> String -> Environment -> IO ExecutionResult
runCWith compiler v s env = let ts = myLexer s in case pProgram ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        runAST tree env compiler

-- Run compiler on the given input code in an empty environment
run :: Compiler -> Verbosity -> String -> IO ExecutionResult
run compiler v s = do
  initEnv <- runInitEmpty
  runWith compiler v s initEnv

-- Run JVM compiler on the given input code in an empty environment using the given configuration
runJVM :: JVMCompilerConfiguration -> Verbosity -> String -> IO ExecutionResult
runJVM opts = run $ compilerJVM opts
