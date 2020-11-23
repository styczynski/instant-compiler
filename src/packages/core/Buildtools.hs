{-# LANGUAGE ScopedTypeVariables #-}
module Buildtools (
    module Development.Shake
    , module Distribution.Simple
    , module Distribution.Simple.Setup
    , module Distribution.Types.HookedBuildInfo
    , module Development.Shake.FilePath
    , decodeString
    , grepShCommand
    , getStackInstallDir
    , executeCommand
    , executeCommandStack
    , executeCommandX
    , executeCommandXEnv
    , executeCommandStackX
    , glob
    , finish
    , cp
    , message
    , executeTasks
    , executeStackBuild
    , executeSubTask
) where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.HookedBuildInfo

import Development.Shake hiding (getEnv)
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shelly (run, liftIO, Sh, shelly, silently, cd, rm_rf, catchany, setenv)
import qualified Shelly
import Data.Text (pack, unpack)
import qualified System.FilePath.Glob as Glob
import Filesystem.Path.CurrentOS (decodeString)
import Text.Regex.TDFA

import System.Environment
import System.IO.Error

import Control.Monad
import Control.Exception.Base

-- Execute stack build CLI command
executeStackBuild :: [String] -> String -> Action Bool
executeStackBuild cmd path = do
    t <- executeCommandStackX cmd path
    (
       case t of
       Just s -> return $ s <> "ERROR"
       Nothing -> executeStackBuild cmd path)

-- Execute stack build CLI command but capture the output
handleStackBuildOutput :: String -> IO (Maybe String)
handleStackBuildOutput output = do
    let matches = filter (\(_, _, _, m) -> not $ null m) $ map (\line -> (line =~ (".*(BUILDTOOLS_BUILD_DIRTY).*" :: String)) :: (String, String, String, [String])) $ lines output
    return (
        case matches of
        ((_, _, _, h:t):_) -> Nothing
        _ -> Just "ERROR")

-- Execute CLI command and grep it with the given regex
grepShCommand :: String -> [String] -> String -> String -> Sh (Maybe [String])
grepShCommand command args cwd regex = do
    cd $ decodeString cwd
    host <- run (decodeString command) $ map pack args
    let matches = filter (\(_, _, _, m) -> not $ null m) $ map (\line -> (line =~ regex) :: (String, String, String, [String])) $ lines $ unpack host
    return (
        case matches of
        ((_, _, _, h:t):_) -> Just $ h:t
        _ -> Nothing)

-- Determine stack installation directory
getStackInstallDir :: String -> IO String
getStackInstallDir path = shelly $ silently $ do
    stackPathResult <- grepShCommand "stack" ["path", "--allow-different-user"] path "local-install-root: (.*)"
    return (
        case stackPathResult of
        Nothing -> ""
        Just (h:_) -> h)

-- Execute CLI command
executeCommand :: String -> [String] -> String -> Action String
executeCommand command args cwd = liftIO $ shelly $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ unpack result

-- Execute Stack CLI command
executeCommandStack :: [String] -> String -> Action String
executeCommandStack args = executeCommand "stack" ("--allow-different-user" : args)

-- Execute Stack CLI command in the given directory
executeCommandX :: String -> [String] -> String -> Action (Maybe String)
executeCommandX command args cwd = liftIO $ catchany (shelly $ do
    cd $ decodeString cwd
    result <- run (decodeString command) $ map pack args
    return $ Just $ unpack result) (\(e :: SomeException) -> do
        handleStackBuildOutput $ show e)

-- Execute Stack CLI command in the given directory with the given env variables
executeCommandXEnv :: String -> [String] -> String -> [(String, String)] -> Action (Maybe String)
executeCommandXEnv command args cwd env = liftIO $ catchany (shelly $ do
    cd $ decodeString cwd
    mapM_ (\(name, value) -> setenv (pack name) (pack value)) env
    result <- run (decodeString command) $ map pack args
    return $ Just $ unpack result) (\(e :: SomeException) -> do
        handleStackBuildOutput $ show e)

-- Execute shake subtask in the given directory
executeSubTask :: String -> String -> Action (Maybe String)
executeSubTask stackVersion path = do
  liftIO $ putStrLn "[!!!] execute shake subtask"
  executeCommandXEnv "bash" ["-c", "unset GHC_PACKAGE_PATH && unset HASKELL_PACKAGE_SANDBOX && unset GHC_ENVIRONMENT && unset HASKELL_DIST_DIR && unset HASKELL_PACKAGE_SANDBOXES && stack exec --cwd " ++ path ++ " -- shake "] "." []

-- Execute stack command
executeCommandStackX :: [String] -> String -> Action (Maybe String)
executeCommandStackX args = executeCommandX "stack" ("--allow-different-user" : args)

-- Glob directory action
glob :: String -> Action [FilePath]
glob = liftIO . Glob.glob

-- Print message action
message :: String -> Action ()
message = liftIO . putStrLn

-- Finish action
finish :: Action ()
finish = do
    liftIO $ setEnv "BUILDTOOLS_BUILD_DIRTY" "TRUE"
    message "Done"

-- Copy action
cp :: FilePath -> FilePath -> Action ()
cp src dest = liftIO $ shelly $ Shelly.cp (decodeString src) (decodeString dest)

-- Execute Shake tasks
executeTasks :: Rules () -> IO ()
executeTasks taskDefs = do
    setEnv "BUILDTOOLS_BUILD_DIRTY" "FALSE"
    shelly $ silently $ do
        liftIO $ shake shakeOptions{shakeFiles="_build"} taskDefs
    buildDirtyEnv <- getEnv "BUILDTOOLS_BUILD_DIRTY"
    Control.Monad.when (buildDirtyEnv == "TRUE") $ ioError $ userError "BUILDTOOLS_BUILD_DIRTY"
