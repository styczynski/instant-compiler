module Analyzer.Analyzer where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import qualified Data.Map.Lazy as M
import           Compiler.Compiler


import           Inference.Inferencer
import           Inference.TypingEnvironment as TE

type Analyzer = Program -> Exec Bool

analyze :: Analyzer
analyze ast@(Program statements) = do
  _ <- return $ inferAST TE.empty TE.initInfer ast
  return True

runAnalyzer :: Program -> Environment -> Analyzer -> IO Bool
runAnalyzer tree env analyzer = do
 r <- runExceptT
   (runReaderT
     (runStateT
       (analyzer tree)
       (CompilerState {}
       )
     )
     (env)
   )
 result <- return
     (case r of
       Left err -> False
       Right (res, _) -> res
     )
 return result