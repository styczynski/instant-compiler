module SSA.SSA where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import           Data.Foldable
import           Inference.Syntax
import qualified Data.Map.Lazy as M

import qualified SSA.Syntax as PSSA

data SSAState = SSAState {
  astNodeID :: Int
}

type SSA
  = StateT (SSAState) (ExceptT String IO)

emptySSAState :: SSAState
emptySSAState = SSAState {
  astNodeID = 1
}

runSSA :: (Program ASTMetadata) -> TypeASTMap -> IO PSSA.Program
runSSA ast types = do
  r <- runExceptT
      (runStateT
        (pssaProgram ast types)
        (emptySSAState)
      )
  result <- return
      (case r of
        -- TODO: Left err -> ...
        Right (res, env) -> res
      )
  return result

pssaProgram :: (Program ASTMetadata) -> TypeASTMap -> SSA PSSA.Program
pssaProgram ast@(Program meta defs) types = do
  defs <- foldrM (\def acc -> pssaTopDef def types acc) [] defs
  return $ PSSA.Program defs

pssaTopDef :: (TopDef ASTMetadata) -> TypeASTMap -> [PSSA.TopLevelDecl] -> SSA [PSSA.TopLevelDecl]
pssaTopDef ast@(FnDef meta retType name args body) types decls = do
  return decls

