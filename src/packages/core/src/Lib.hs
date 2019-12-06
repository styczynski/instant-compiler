module Lib where

import Compiler.Compiler
import Analyzer.Analyzer
import Syntax.Base
import Control.Exception

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Verbosity = Int

runInit :: Environment -> IO Environment
runInit env = return env

runInitEmpty :: IO Environment
runInitEmpty = runInit emptyEnv

runWith :: Compiler -> Verbosity -> String -> Environment -> IO ExecutionResult
runWith compiler v s env = do
    r <- runCWith compiler v s env
    return r

runCWith :: Compiler -> Verbosity -> String -> Environment -> IO ExecutionResult
runCWith compiler v s env = let ts = myLexer s in case pProgram ts of
          Bad s    -> return $ FailedParse $ show s
          Ok  tree -> do
                        _ <- runAnalyzer tree env analyze
                        _ <- return $ assert False 0
                        res <- runAST tree env compiler
                        return res

run :: Compiler -> Verbosity -> String -> IO ExecutionResult
run compiler v s = do
  initEnv <- runInitEmpty
  runWith compiler v s initEnv