module InscCli where

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath

import Inference.Syntax

import Lib
import Compiler.Compiler
import SSA.Compiler
import Syntax.Base

compilerInit :: (Program ASTMetadata, ASTNode ASTMetadata)
compilerInit = (Program EmptyMetadata [], ASTNone EmptyMetadata)

runFile :: Compiler (Program ASTMetadata) (ASTNode ASTMetadata) -> Verbosity -> FilePath -> IO ()
runFile compiler v f = putStrLn f >> readFile f >>= runBlockI compiler v

callCompiler :: SSACompilerConfiguration -> Verbosity -> String -> IO String
callCompiler opt v = runBlockC (compilerSSA opt) v

runBlockC :: Compiler (Program ASTMetadata) (ASTNode ASTMetadata) -> Verbosity -> String -> IO String
runBlockC compiler v s = do
  initEnv0 <- runInitEmpty compilerInit
  result <- runWith compilerInit compiler v s initEnv0
  case result of
     FailedCompilation s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Compiled out env -> return out

runBlockI :: Compiler (Program ASTMetadata) (ASTNode ASTMetadata) -> Verbosity -> String -> IO ()
runBlockI compiler v s = do
  initEnv0 <- runInitEmpty compilerInit
  result <- runWith compilerInit compiler v s initEnv0
  case result of
     FailedCompilation s -> do
                    hPutStrLn stderr s
                    exitFailure
     FailedParse s  -> do
                    hPutStrLn stderr s
                    exitFailure
     Compiled out env -> do
                    putStrLn out

execContents compiler v = getContents >>= runBlockI compiler v

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

compilerConf :: (Maybe String) -> Bool -> SSACompilerConfiguration
compilerConf inputFile shouldRun = case inputFile of
  Nothing -> defaultSSACompilerConfiguration { ssaRunProgram = shouldRun }
  (Just path) -> defaultSSACompilerConfiguration { ssaProgramName = (takeBaseName path), ssaOutputPath = (takeDirectory path), ssaRunProgram = shouldRun }

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs file verbosity shouldRun) = case (verbosity, file, shouldRun) of
  (v, "stdin", shouldRun) -> execContents (compilerSSA $ compilerConf Nothing shouldRun) v
  (v, src, shouldRun) -> runFile (compilerSSA $ compilerConf (Just src) shouldRun) v src
mainEntry _ = return ()