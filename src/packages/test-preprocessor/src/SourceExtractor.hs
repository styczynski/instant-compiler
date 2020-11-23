module SourceExtractor where

import Text.Regex.TDFA
import Data.Text (strip, pack, unpack)
import Data.Array (elems)

regexTagTest = "\\(\\*\\* *Test: +(.+) +\\*\\*\\)"
regexTagDesribe = "\\(\\*\\* *Describe: +(.+) +\\*\\*\\)"
regexTagError = "\\(\\*\\* *Throws: +(.+) +\\*\\*\\)"
regexTagSkip = "\\(\\*\\* *Skip: +(.+) +\\*\\*\\)"
regexTagOutput = "\\(\\*\\* *Output: +(.+) +\\*\\*\\)"

data SourceMetadata = SourceMetadata
  { testDescription :: String, testName :: String, errorRegex :: Maybe String, shouldSkip :: Bool, expectedOutput :: String }

replaceAll :: String -> String -> String -> String
replaceAll regex new_str str  =
    let parts = concatMap elems (str  =~  regex :: [MatchArray])
    in foldl (replace' new_str) str (reverse parts)

  where
     replace' :: [a] -> [a] -> (Int, Int) -> [a]
     replace' new list (shift, l)   =
        let (pre, post) = splitAt shift list
        in pre ++ new ++ drop l post

removeAllTags :: String -> String
removeAllTags = replaceAll "\\(\\*\\* *(.+): +(.+) +\\*\\*\\)" ""

extractTagUsing :: String -> SourceMetadata -> String -> (SourceMetadata -> [String] -> SourceMetadata) -> IO SourceMetadata
extractTagUsing input meta regex foldFn = do
    (a, b, c, descrMatchGroups) <- return ((input =~ regex) :: (String, String, String, [String]))
    return $ foldl foldFn meta [descrMatchGroups]

extractTagDescriptionFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagDescriptionFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testDescription = unpack $ strip $ pack h }

extractTagTestFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagTestFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testName = unpack $ strip $ pack h }

extractTagErrorFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagErrorFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { errorRegex = Just $ unpack $ strip $ pack h }

extractTagSkipFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagSkipFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { shouldSkip = unpack (strip $ pack h) == "Yes" }

extractTagOutputFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagOutputFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { expectedOutput = unpack $ strip $ pack h }

extractTestMetadata :: String -> IO SourceMetadata
extractTestMetadata input = do
    let initMeta = SourceMetadata { testDescription = "", testName = "", errorRegex = Nothing, shouldSkip = False, expectedOutput = "" }
    meta0 <- extractTagUsing input initMeta regexTagDesribe extractTagDescriptionFn
    meta1 <- extractTagUsing input meta0 regexTagTest extractTagTestFn
    meta2 <- extractTagUsing input meta1 regexTagError extractTagErrorFn
    meta3 <- extractTagUsing input meta2 regexTagSkip extractTagSkipFn
    extractTagUsing input meta3 regexTagOutput extractTagOutputFn
