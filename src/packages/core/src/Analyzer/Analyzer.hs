{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

type Analyzer r t = r -> t -> Exec String

class (AST r t) => Analyzable r t where
  emptyPayload :: t
  emptyInfer :: r -> t -> InferState r t
  emptyInfer r _ = InferState { count       = 0
                         , tCount        = 0
                         , tagMap        = M.empty
                         , inferTrace    = []
                         , lastInferExpr = EmptyPayload
                         , root = r
                         }
  analyzer :: Analyzer r t
  analyzer ast t0 = do
    r <- liftIO $ inferAST TE.empty (emptyInfer ast t0) ast
    str <- return
         (case r of
           Left err -> (show err)
           Right (scheme, _, _) -> (show scheme)
         )
    return str
  analyze :: r -> t -> Environment -> IO String
  analyze tree t0 env = do
   r <- runExceptT
     (runReaderT
       (runStateT
         (analyzer tree t0)
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