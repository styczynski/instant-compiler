module InscCli where

import Options.Applicative
import Data.Semigroup ((<>))

import System.FilePath

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Lib
import Compiler.Compiler
import JVM.JVM

--| Run compiler on a given file path
runFile :: Compiler -> Verbosity -> FilePath -> IO ()
runFile compiler v f = putStrLn f >> readFile f >>= runBlock compiler v

--| Run compiler with the specified settings on a given input code
callCompiler :: JVMCompilerConfiguration -> Verbosity -> String -> IO String
callCompiler opt = runBlockC (compilerJVM opt)

--| Run given compiler on empty environment and given code and return output code
runBlockC :: Compiler -> Verbosity -> String -> IO String
runBlockC compiler v s = do
  initEnv0 <- runInitEmpty
  result <- runWith compiler v s initEnv0
  case result of
     FailedCompilation s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Compiled out env -> return out

--| Run given compiler on empty environment and given code and print output code
runBlockI :: Compiler -> Verbosity -> String -> IO ()
runBlockI compiler v s = do
  initEnv0 <- runInitEmpty
  result <- runWith compiler v s initEnv0
  case result of
     FailedCompilation s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Compiled out env -> do
                    putStrLn out

--| Easy way to change the default behaviour
runBlock = runBlockI

--| Load input from stdin and run given compiler on it
execContents compiler v = getContents >>= runBlock compiler v

--| CLI parameters
data MainArgs = MainArgs
  { file :: String
  , verbosity :: Int
  , shouldRun :: Bool }

--| Parse CLI parameters
parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> argument str (metavar "FILE")
  <*> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> switch (short 'r' <>
     long "run" <>
     help "Run program after compilation")

--| Create compiler configuration
compilerConf :: Maybe String -> Bool -> JVMCompilerConfiguration
compilerConf inputFile shouldRun = case inputFile of
  Nothing -> defaultJVMCompilerConfiguration { jvmRunProgram = shouldRun }
  (Just path) -> defaultJVMCompilerConfiguration { jvmProgramName = takeBaseName path, jvmOutputPath = takeDirectory path, jvmRunProgram = shouldRun }

--| CLI entrypoint
mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs file verbosity shouldRun) = case (verbosity, file, shouldRun) of
  (v, "stdin", shouldRun) -> execContents (compilerJVM $ compilerConf Nothing shouldRun) v
  (v, src, shouldRun) -> runFile (compilerJVM $ compilerConf (Just src) shouldRun) v src
mainEntry _ = return ()
