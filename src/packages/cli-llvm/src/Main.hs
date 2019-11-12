module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import InscCli

main :: IO ()
main = mainEntry =<< execParser opts
  where
    opts = info (parseMainArgs <**> helper)
      ( fullDesc
      <> progDesc "Tiny Ocaml interprter for Haskell"
      <> header "Piotr Styczynski 2019" )