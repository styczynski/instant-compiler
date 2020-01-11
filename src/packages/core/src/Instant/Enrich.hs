module Instant.Enrich where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import           Inference.Syntax
import qualified Data.Map.Lazy as M

data EnrichState = EnrichStateEmpty

type Enrich
  = StateT (EnrichState) (ExceptT String IO)

enrichInner :: (AST r t) => (r, t) -> ASTMetadata -> ASTMetadata
enrichInner _ m = m

enrich :: (AST r t, Functor f) => (r, t) -> (f ASTMetadata) -> Enrich (f ASTMetadata)
enrich s0 f = return $ fmap (enrichInner s0) f

runEnrich :: (AST r t, Functor f) => (r, t) -> (f ASTMetadata) -> IO (f ASTMetadata)
runEnrich s0 ast = do
  r <- runExceptT
      (runStateT
        (enrich s0 ast)
        (EnrichStateEmpty)
      )
  result <- return
      (case r of
        -- TODO: Left err -> ...
        Right (res, env) -> res
      )
  return result