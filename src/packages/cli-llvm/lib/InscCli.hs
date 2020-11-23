module InscCli where

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath

import Lib
import Compiler.Compiler
import LLVM.LLVM

-- Run compiler on a given file path
runFile :: Compiler -> Verbosity -> FilePath -> IO ()
runFile compiler v f = putStrLn f >> readFile f >>= runBlock compiler v

-- Run compiler with the specified settings on a given input code
callCompiler :: LLVMCompilerConfiguration -> Verbosity -> String -> IO String
callCompiler opt = runBlockC (compilerLLVM opt)

-- Run given compiler on empty environment and given code and return output code
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

-- Run given compiler on empty environment and given code and print output code
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

-- Easy way to change the default behaviour
runBlock = runBlockI

-- Load input from stdin and run given compiler on it
execContents compiler v = getContents >>= runBlock compiler v

-- CLI parameters
data MainArgs = MainArgs
  { file :: String
  , verbosity :: Int
  , shouldRun :: Bool }

-- Parse CLI parameters
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

-- Create compiler configuration
compilerConf :: Maybe String -> Bool -> LLVMCompilerConfiguration
compilerConf inputFile shouldRun = case inputFile of
  Nothing -> defaultLLVMCompilerConfiguration { llvmRunProgram = shouldRun }
  (Just path) -> defaultLLVMCompilerConfiguration { llvmProgramName = takeBaseName path, llvmOutputPath = takeDirectory path, llvmRunProgram = shouldRun }

-- CLI entrypoint
mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs file verbosity shouldRun) = case (verbosity, file, shouldRun) of
  (v, "stdin", shouldRun) -> execContents (compilerLLVM $ compilerConf Nothing shouldRun) v
  (v, src, shouldRun) -> runFile (compilerLLVM $ compilerConf (Just src) shouldRun) v src
mainEntry _ = return ()
