module InscCli where

import Options.Applicative
import Data.Semigroup ((<>))

import System.IO ( stdin, stderr, hPutStrLn, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Lib
import Compiler.Compiler
import JVM.JVM

runFile :: Compiler -> Verbosity -> FilePath -> IO ()
runFile compiler v f = putStrLn f >> readFile f >>= runBlock compiler v

callCompiler :: JVMCompilerConfiguration -> Verbosity -> String -> IO String
callCompiler opt v = runBlockC (compilerJVM opt) v

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
  { verbosity :: Int
  , file :: String }

parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> strOption
      ( long "file"
      <> short 'f'
      <> showDefault
      <> value "stdin"
      <> help "File to load or stdin to load standard input"
      <> metavar "FILENAME" )

mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity file) = case (verbosity, file) of
  (v, "stdin") -> execContents defaultCompilerJVM v
  (v, src) -> runFile defaultCompilerJVM v src
mainEntry _ = return ()