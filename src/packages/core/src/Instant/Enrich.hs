module Instant.Enrich where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import           Inference.Syntax
import qualified Data.Map.Lazy as M

data EnrichState = EnrichState {
  astNodeID :: Int
}

type Enrich
  = StateT (EnrichState) (ExceptT String IO)

emptyEnrichState :: EnrichState
emptyEnrichState = EnrichState {
  astNodeID = 1
}

nextNodeID :: Enrich Int
nextNodeID = do
  env <- get
  nextID <- return $ astNodeID env
  put env { astNodeID = nextID+1 }
  return $ nextID

enrichInner :: (AST r t) => (r, t) -> ASTMetadata -> Enrich ASTMetadata
enrichInner _ EmptyMetadata = do
  id <- nextNodeID
  return $ ASTMetadata (ASTRef id) (ASTPos 0 0 0)
enrichInner _ (ASTMetadata _ p) = do
  id <- nextNodeID
  return $ ASTMetadata (ASTRef id) p

enrich :: (AST r t, ASTMutable f) => (r, t) -> (f ASTMetadata) -> Enrich (f ASTMetadata)
enrich s0 ast = astMap (enrichInner s0) ast

runEnrich :: (AST r t, ASTMutable f) => (r, t) -> (f ASTMetadata) -> IO (f ASTMetadata)
runEnrich s0 ast = do
  r <- runExceptT
      (runStateT
        (enrich s0 ast)
        (emptyEnrichState)
      )
  result <- return
      (case r of
        -- TODO: Left err -> ...
        Right (res, env) -> res
      )
  return result