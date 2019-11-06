module JVM.JVM where

import Compiler.Compiler
import Syntax.Base

compilerJVM :: Program -> Exec (String, Environment)
compilerJVM _ = do
  return ("out", emptyEnv)