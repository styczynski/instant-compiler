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

runFile :: Compiler -> Verbosity -> FilePath -> IO ()
runFile compiler v f = putStrLn f >> readFile f >>= runBlock compiler v

callCompiler :: LLVMCompilerConfiguration -> Verbosity -> String -> IO String
callCompiler opt v = runBlockC (compilerLLVM opt) v

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

runBlock = runBlockI

execContents compiler v = getContents >>= runBlock compiler v

data MainArgs = MainArgs
  { file :: String
  , verbosity :: Int
  , shouldRun :: Bool }

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

compilerConf :: (Maybe String) -> Bool -> LLVMCompilerConfiguration
compilerConf inputFile shouldRun = case inputFile of
  Nothing -> defaultLLVMCompilerConfiguration { llvmRunProgram = shouldRun }
  (Just path) -> defaultLLVMCompilerConfiguration { llvmProgramName = (takeBaseName path), llvmOutputPath = (takeDirectory path), llvmRunProgram = shouldRun }

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs file verbosity shouldRun) = case (verbosity, file, shouldRun) of
  (v, "stdin", shouldRun) -> execContents (compilerLLVM $ compilerConf Nothing shouldRun) v
  (v, src, shouldRun) -> runFile (compilerLLVM $ compilerConf (Just src) shouldRun) v src
mainEntry _ = return ()