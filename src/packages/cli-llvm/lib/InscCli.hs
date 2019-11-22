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
  , verbosity :: Int }

parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> argument str (metavar "FILE")
  <*> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )

compilerConf :: (Maybe String) -> LLVMCompilerConfiguration
compilerConf inputFile = case inputFile of
  Nothing -> defaultLLVMCompilerConfiguration
  (Just path) -> defaultLLVMCompilerConfiguration { llvmProgramName = (takeBaseName path), llvmOutputPath = (takeDirectory path) }

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs file verbosity) = case (verbosity, file) of
  (v, "stdin") -> execContents (compilerLLVM $ compilerConf Nothing) v
  (v, src) -> runFile (compilerLLVM $ compilerConf $ Just src) v src
mainEntry _ = return ()