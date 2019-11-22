{-# LANGUAGE QuasiQuotes #-}

module Preprocessor where

import SourceExtractor

import Text.Heterocephalus
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Data.ByteString.Lazy.UTF8 hiding (lines, length)

import Data.List

import System.FilePath.Posix
import System.Directory
import System.IO

import Data.Text (strip, pack, unpack)

import Control.Monad

type Verbosity = Int

preprocessDirectory :: String -> String -> FilePath -> FilePath -> IO ()
preprocessDirectory lang prefix out dir = do
    _ <- putStrLn $ "Scan dir " ++ dir
    fileNames <- getDirectoryContents dir
    entriesNames <- filterM (\input -> return $ input `notElem` [".",".."]) fileNames
    entries <- return $ map (\name -> dir </> name) entriesNames
    fineFiles <- filterM (\input -> doesFileExist input) entries
    fineDirs <- filterM (\input -> doesDirectoryExist input) entries
    _ <- mapM (preprocessDirectory lang prefix out) fineDirs
    mapM (preprocessFile lang prefix out) fineFiles >>= \_ -> return ()

preprocessFile :: String -> String -> FilePath -> FilePath -> IO ()
preprocessFile lang prefix outPath file = do
    z <- putStrLn $ "Open file " ++ file
    name <- return $ takeBaseName file
    specName <- return $ name ++ "Spec"
    specNameFile <- return $ specName ++ ".hs"
    testName <- return $ outPath </> specNameFile
    fileContent <- readFile file
    out <- preprocessTest lang prefix specName fileContent
    createDirectoryIfMissing True outPath
    writeFile testName out

preprocessTest :: String -> String -> String -> String -> IO String
preprocessTest lang prefix specName input = do
    (SourceMetadata { testDescription = testDescription, testName = testName, errorRegex = errorRegex, shouldSkip = shouldSkip, expectedOutput = expectedOutput }) <- extractTestMetadata input
    errorRegexStr <- return $ maybe "" (\x -> x) errorRegex
    multilineStringStart <- return "[r|"
    multilineStringEnd <- return "|]"
    indentedInput <- return $ intercalate "\n" $ filter (\line -> length (unpack $ strip $ pack line) > 0) $ map (\line -> removeAllTags $ "         " ++ (unpack $ strip $ pack line)) $ lines input
    return $ toString $ renderMarkup $
      [compileText|
{-# LANGUAGE QuasiQuotes #-}
module #{prefix}#{specName} where
import Text.RawString.QQ

%{ if (not (errorRegex == Nothing)) }
import Text.Regex.TDFA
%{ endif }

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

%{ if (lang == "jvm") }
import JVM.JVM
%{ endif }
%{ if (lang == "llvm") }
import LLVM.LLVM
%{ endif }
import InscCli (callCompiler)

spec = do
  describe "#{specName}: #{testName}" $ do
    %{ if (shouldSkip) } xit %{ else } it %{ endif }"#{testDescription}" $ do
%{ if (lang == "jvm") }
      options <- return $ JVMCompilerConfiguration { jvmLibLocation = "../../..", jvmBinLocation = "../../..", jvmRunProgram = True, jvmProgramName = "#{specName}", jvmOutputPath = "./insc_build" }
%{ endif }
%{ if (lang == "llvm") }
      options <- return $ LLVMCompilerConfiguration { llvmLibLocation = "../../..", llvmRunProgram = True, llvmProgramName = "#{specName}", llvmOutputPath = "./insc_build" }
%{ endif }
      output <- callCompiler options 0 #{multilineStringStart}
#{indentedInput}
      #{multilineStringEnd}
      output `shouldBe` #{expectedOutput}
      |]
