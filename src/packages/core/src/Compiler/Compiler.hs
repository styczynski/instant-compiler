module Compiler.Compiler where

import           Syntax.Base
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Lazy
import qualified Data.Map.Lazy as M

{-|
  Internal state of the compiler
-}
data CompilerState = CompilerState {}

{-|
  Environment is the semantic environment for the given program instruction
-}
data Environment = Environment {
  --| Mapping from a variable number into its location in memory
  store :: Map Int Location,
  --| Mapping from the variable name into the variable itself
  scope :: Map String Variable,
  --| Free LV index is used to allocate the variable in the store
  freeLVIndex :: Int,
  --| Free variable ID is used when creating new variables
  freeVarID :: Int,
  --| Var no is used to generate variable names
  freeVarNameNo :: Int
}

--| Location of the variable
newtype Location = Local Int

--| Variable metadata
newtype Variable = VarID Int

--| Define initial environment conditions
emptyEnv = Environment {
  store = empty,
  scope = empty,
  freeLVIndex = 0,
  freeVarID = 0,
  freeVarNameNo = 0
}

--| The result of the compiler execution
data ExecutionResult = FailedParse String | FailedCompilation String | Compiled String Environment

--| Monad defining the compiler execution
type Exec
  = StateT CompilerState (ReaderT Environment (ExceptT String IO))

--| Compiler definition
type Compiler = Program -> Exec (String, Environment)

--| Create variable name using the given prefix and index
getUniqueNameFrom :: String -> Int -> String
getUniqueNameFrom prefix index = prefix ++ "__var_" ++ show index

--| Create unique variable name using the given prefix
uniqueNameFromRaw :: String -> Environment -> (String, Environment)
uniqueNameFromRaw prefix env =
  let newName = prefix ++ "_" ++ show (freeVarNameNo env) in
    (newName, env { freeVarNameNo = freeVarNameNo env + 1 })

--| Get unique variable name index and increment it returning new environment
uniqueNameIndex :: Environment -> (Int, Environment)
uniqueNameIndex env =
  let newNameIndex = freeVarNameNo env in
    (newNameIndex, env { freeVarNameNo = freeVarNameNo env + 1 })

--| Create unique variable name
uniqueName :: Environment -> (String, Environment)
uniqueName = uniqueNameFromRaw "var"

--| Create unique variable name with a given prefix
uniqueNameFrom :: String -> Environment -> (String, Environment)
uniqueNameFrom prefix = uniqueNameFromRaw (prefix ++ "__var")

--| Allocate space for variable with the given ID
allocate :: Int -> Environment -> (Location, Environment)
allocate id env =
  let freeIndex = freeLVIndex env in
    (Local freeIndex, env { freeLVIndex = freeLVIndex env + 1, store = insert id (Local freeIndex) (store env) })

--| Allocate space for variable with the given ID and location ID
allocateAt :: Int -> Int -> Environment -> (Location, Environment)
allocateAt id freeIndex env = (Local freeIndex, env { store = insert id (Local freeIndex) (store env) })

--| Create new variable without assigned location, inside the environment
define :: String -> Environment -> (Int, Environment)
define name env =
  let freeID = freeVarID env in
    (freeID, env { freeVarID = freeVarID env + 1, scope = insert name (VarID freeID) (scope env) })

--| Create new variable inside the environment and allocate space for it
defineAndAlloc :: String -> Environment -> (Location, Environment)
defineAndAlloc name env =
  let var = getVarFromScope name env in case var of
    Nothing -> let (newID, newEnv) = define name env in allocate newID newEnv
    Just (VarID v)  -> let loc = getVarLocByID (VarID v) env in case loc of
      Nothing -> let (newID, newEnv) = define name env in allocate newID newEnv
      (Just loc) -> (loc, env)

--| Get variable location using its ID
getVarLocByID :: Variable -> Environment -> Maybe Location
getVarLocByID (VarID id) env = M.lookup id (store env)

--| Get variable by name from the current scope
getVarFromScope :: String -> Environment -> Maybe Variable
getVarFromScope name env = M.lookup name (scope env)

--| Get variable location by name from the current scope
getVar :: String -> Environment -> Maybe Location
getVar name env = let var = getVarFromScope name env in case var of
  Just v -> getVarLocByID v env
  Nothing -> Nothing

--| Compile the program using specified compiler
runAST :: Program -> Environment -> Compiler -> IO ExecutionResult
runAST tree env compiler = do
  r <- runExceptT
    (runReaderT
      (runStateT
        (compiler tree)
        (CompilerState {}
        )
      )
      env
    )
  return
    (case r of
      Left err -> FailedCompilation err
      Right ((res, env), _) ->
        Compiled res env
    )
