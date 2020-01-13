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
import           Inference.Errors
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
                         , root = r
                         , typeMap = M.empty
                         }
  analyzer :: Analyzer r t
  analyzer ast t0 = do
    r <- liftIO $ inferAST TE.empty (emptyInfer ast t0) ast
    str <- liftIO $ liftIO $ liftIO $ (case r of
       Left err -> (typeErrorToStr (ast, t0) err)
       Right (scheme, _, _) -> return (show scheme)
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
         Left err -> err
         Right (res, _) -> res
       )
   putStrLn result
   return result