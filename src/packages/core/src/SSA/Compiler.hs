{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSA.Compiler where
import Text.RawString.QQ

import X86.Examples

import Compiler.Compiler
import Syntax.Base

import Shelly

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad
import Data.Array
import qualified Data.Map as M
import qualified Data.Text as T

import Inference.Syntax
import Analyzer.Analyzer
import Lib

import SSA.Syntax

instance Analyzable (Program ASTMetadata) (ASTNode ASTMetadata) where
  emptyPayload = ASTNone EmptyMetadata

instance Compilable (Program ASTMetadata) (ASTNode ASTMetadata) where
  parse (_, t0) source = let ts = myLexer source in case pProgram ts of
    Bad e -> Left $ FailedParse $ show source
    Ok r -> Right (fmap (\_ -> EmptyMetadata) r, t0)

data SSACompilerConfiguration = SSACompilerConfiguration {
  ssaLibLocation :: String,
  ssaRunProgram :: Bool,
  ssaProgramName :: String,
  ssaOutputPath :: String
}

defaultSSACompilerConfiguration :: SSACompilerConfiguration
defaultSSACompilerConfiguration = SSACompilerConfiguration {
  ssaLibLocation = ".",
  ssaRunProgram = False,
  ssaProgramName = "main",
  ssaOutputPath = "."
}

defaultCompilerSSA :: Compiler (Program ASTMetadata) (ASTNode ASTMetadata)
defaultCompilerSSA p = compilerSSA defaultSSACompilerConfiguration p

compile :: (Program ASTMetadata) -> Exec ([SSANode], Environment)
compile (Program _ statements) = do
  env <- ask
  return ([], env)

compilerSSA :: SSACompilerConfiguration -> Compiler (Program ASTMetadata) (ASTNode ASTMetadata)
compilerSSA opts (program@(Program _ statements), _) = do
  _ <- liftIO $ putStrLn "Compile SSA code"
  (compiledProgram, env) <- compile program
  content <- return $ show compiledProgram
  return (content, env)
