module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Preprocessor

--| CLI parameters
data MainArgs = MainArgs
  { verbosity :: Int
  , inputDirectory :: String
  , lang :: String
  , outputDirectory :: String
  , testPrefix :: String }

--| Parse CLI parameters
parseMainArgs :: Parser MainArgs
parseMainArgs = MainArgs
  <$> option auto
    ( long "verbosity"
    <> help "Set verbosity level of the program"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> strOption
      ( long "inputDirectory"
      <> short 'i'
      <> showDefault
      <> value "examples"
      <> help "Direcotry to look for tests input code"
      <> metavar "INPUT_DIRECTORY" )
  <*> strOption
        ( long "outputLanguage"
        <> short 'l'
        <> showDefault
        <> value "jvm"
        <> help "Output language (jvm/llvm)"
        <> metavar "INPUT_DIRECTORY" )
  <*> strOption
        ( long "outputDirectory"
        <> short 'o'
        <> showDefault
        <> value "test"
        <> help "Direcotry to look for tests input code"
        <> metavar "OUTPUT_DIRECTORY" )
  <*> strOption
          ( long "testPrefix"
          <> short 'p'
          <> showDefault
          <> value ""
          <> help "Direcotry to look for tests input code"
          <> metavar "TEST_PREFIX" )

--| Test preprocessor main function
main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Haskell HSpec suites generator for Instant language"
      <> header "Piotr Styczynski 2019" )

--| Test preprocessor CLI entrypoint
mainEntry :: MainArgs -> IO ()
mainEntry (MainArgs verbosity file lang out prefix) = case (verbosity, file, lang, out, prefix) of
  (v, dir, lang, out, prefix) -> preprocessDirectory lang prefix out dir
mainEntry _ = return ()