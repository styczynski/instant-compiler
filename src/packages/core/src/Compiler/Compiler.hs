module Compiler.Compiler where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import qualified Data.Map.Lazy as M

data CompilerState = CompilerState {}

data Environment = Environment {
  store :: Map Int Location,
  scope :: Map String Variable,
  freeLVIndex :: Int,
  freeVarID :: Int,
  freeVarNameNo :: Int
}

data Location = Local Int

data Variable = VarID Int

emptyEnv = Environment {
  store = empty,
  scope = empty,
  freeLVIndex = 0,
  freeVarID = 0,
  freeVarNameNo = 0
}

data ExecutionResult = FailedParse String | FailedCompilation String | Compiled String Environment
type Exec
  = StateT (CompilerState) (ReaderT (Environment) (ExceptT String IO))

type Compiler = Program -> Exec (String, Environment)

getUniqueNameFrom :: String -> Int -> String
getUniqueNameFrom prefix index = prefix ++ "__var_" ++ show index

uniqueNameFromRaw :: String -> Environment -> (String, Environment)
uniqueNameFromRaw prefix env =
  let newName = prefix ++ "_" ++ show (freeVarNameNo env) in
    (newName, env { freeVarNameNo = (freeVarNameNo env) + 1 })

uniqueNameIndex :: Environment -> (Int, Environment)
uniqueNameIndex env =
  let newNameIndex = freeVarNameNo env in
    (newNameIndex, env { freeVarNameNo = (freeVarNameNo env) + 1 })

uniqueName :: Environment -> (String, Environment)
uniqueName env = uniqueNameFromRaw "var" env

uniqueNameFrom :: String -> Environment -> (String, Environment)
uniqueNameFrom prefix env = uniqueNameFromRaw (prefix ++ "__var") env

allocate :: Int -> Environment -> (Location, Environment)
allocate id env =
  let freeIndex = freeLVIndex env in
    ((Local freeIndex), env { freeLVIndex = (freeLVIndex env) + 1, store = insert id (Local freeIndex) (store env) })

allocateAt :: Int -> Int -> Environment -> (Location, Environment)
allocateAt id freeIndex env = ((Local freeIndex), env { store = insert id (Local freeIndex) (store env) })

define :: String -> Environment -> (Int, Environment)
define name env =
  let freeID = freeVarID env in
    (freeID, env { freeVarID = (freeVarID env) + 1, scope = insert name (VarID freeID) (scope env) })

defineAndAlloc :: String -> Environment -> (Location, Environment)
defineAndAlloc name env =
  let var = getVarFromScope name env in case var of
    Nothing -> let (newID, newEnv) = define name env in allocate newID newEnv
    Just (VarID v)  -> let loc = getVarLocByID (VarID v) env in case loc of
      Nothing -> let (newID, newEnv) = define name env in allocate newID newEnv
      (Just loc) -> (loc, env)

getVarLocByID :: Variable -> Environment -> Maybe Location
getVarLocByID (VarID id) env = M.lookup id (store env)

getVarFromScope :: String -> Environment -> Maybe Variable
getVarFromScope name env = M.lookup name (scope env)

getVar :: String -> Environment -> Maybe Location
getVar name env = let var = getVarFromScope name env in case var of
  Just v -> getVarLocByID v env
  Nothing -> Nothing

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