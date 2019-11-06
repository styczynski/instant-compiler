module Compiler.Compiler where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader

data CompilerState = CompilerState {}

data Environment = Environment {}

emptyEnv = Environment {}

data ExecutionResult = FailedParse String | FailedCompilation String | Compiled String Environment
type Exec
  = StateT (CompilerState) (ReaderT (Environment) (ExceptT String IO))

type Compiler = Program -> Exec (String, Environment)

runAST :: Program -> Environment -> Compiler -> IO ExecutionResult
runAST tree env compiler = do
  r <- runExceptT
    (runReaderT
      (runStateT
        (compiler tree)
        (CompilerState {}
        )
      )
      (env)
    )
  result <- return
    (case r of
      Left err -> FailedCompilation err
      Right ((res, env), _) ->
        Compiled res env
    )
  return result