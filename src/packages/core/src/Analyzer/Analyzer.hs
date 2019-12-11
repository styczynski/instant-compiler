module Analyzer.Analyzer where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import qualified Data.Map.Lazy as M
import           Compiler.Compiler

import           Inference.Syntax
import           Inference.Inferencer
import           Inference.TypingEnvironment as TE

type Analyzer t = t -> Exec String

analyze :: (Traceable t) => Analyzer t
analyze ast = do
  r <- liftIO $ inferAST TE.empty TE.initInfer ast
  str <- return
       (case r of
         Left err -> (show err)
         Right (scheme, _, _) -> (show scheme)
       )
  return str

runAnalyzer :: (Traceable t) => t -> Environment -> Analyzer t -> IO String
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
       Left err -> (show err)
       Right (res, _) -> res
     )
 putStrLn $ "inference: " ++ result
 return result