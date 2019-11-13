module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import InscCli

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Instant compiler for LLVM backend"
      <> header "Piotr Styczynski 2019" )