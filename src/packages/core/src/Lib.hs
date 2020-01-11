{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lib where

import Compiler.Compiler
import Analyzer.Analyzer
import Syntax.Base
import Control.Exception
import Control.Monad.IO.Class

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Verbosity = Int

class (Analyzable r t) => Compilable r t where
  enrich :: (r, t) -> IO r

  parse :: (r, t) -> String -> Either ExecutionResult (r, t)

  runInit :: (r, t) -> Environment -> IO Environment
  runInit _ env = return env

  runInitEmpty :: (r, t) -> IO Environment
  runInitEmpty s0 = runInit s0 emptyEnv

  runWith :: (r, t) -> Compiler r t -> Verbosity -> String -> Environment -> IO ExecutionResult
  runWith s0 compiler v s env = do
      r <- runCWith s0 compiler v s env
      return r

  runCWith :: (r, t) -> Compiler r t -> Verbosity -> String -> Environment -> IO ExecutionResult
  runCWith (r0, t0) compiler v s env = case parse (r0, t0) s of --let ts = myLexer s in case pProgram ts of
            Left e    -> return $ e --FailedParse $ show s
            Right  (tree, t0) -> do
                tree <- liftIO $ enrich (tree, t0)
                _ <- analyze tree t0 env
                _ <- return $ assert False 0
                res <- runAST (r0, t0) env compiler
                return res

  run :: (r, t) -> Compiler r t -> Verbosity -> String -> IO ExecutionResult
  run s0 compiler v s = do
    initEnv <- runInitEmpty s0
    runWith s0 compiler v s initEnv