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

import Data.Maybe
import Data.Text (strip, pack, unpack)

import Control.Monad

-- Verbosity level
type Verbosity = Int

-- Run test preprocessor on entire directory
preprocessDirectory :: String -> String -> FilePath -> FilePath -> IO ()
preprocessDirectory lang prefix out dir = do
    _ <- putStrLn $ "Scan dir " ++ dir
    fileNames <- getDirectoryContents dir
    entriesNames <- filterM (\input -> return $ input `notElem` [".",".."]) fileNames
    let entries = map (dir </>) entriesNames
    fineFiles <- filterM doesFileExist entries
    fineDirs <- filterM doesDirectoryExist entries
    mapM_ (preprocessDirectory lang prefix out) fineDirs
    mapM_ (preprocessFile lang prefix out) fineFiles

-- Run test preprocessor on a single test file
preprocessFile :: String -> String -> FilePath -> FilePath -> IO ()
preprocessFile lang prefix outPath file = do
    z <- putStrLn $ "Open file " ++ file
    let name = takeBaseName file
    let specName = name ++ "Spec"
    let specNameFile = specName ++ ".hs"
    let testName = outPath </> specNameFile
    fileContent <- readFile file
    out <- preprocessTest lang prefix specName fileContent
    createDirectoryIfMissing True outPath
    writeFile testName out

-- Run test preprocessor on a input string with test content
preprocessTest :: String -> String -> String -> String -> IO String
preprocessTest lang prefix specName input = do
    SourceMetadata { testDescription = testDescription, testName = testName, errorRegex = errorRegex, shouldSkip = shouldSkip, expectedOutput = expectedOutput } <- extractTestMetadata input
    let errorRegexStr = Data.Maybe.fromMaybe "" errorRegex
    let multilineStringStart = "[r|"
    let multilineStringEnd = "|]"
    let indentedInput = intercalate "\n" $ filter (\line -> not $ null (unpack $ strip $ pack line)) $ map (removeAllTags . (++ "         ") . unpack . strip . pack) $ lines input
    return $ toString $ renderMarkup
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
