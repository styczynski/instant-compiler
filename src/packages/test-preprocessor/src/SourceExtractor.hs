module SourceExtractor where

import Text.Regex.TDFA
import Data.Text (strip, pack, unpack)
import Data.Array (elems)

--| Regexes that match the test content
--| Define test name
regexTagTest = "\\(\\*\\* *Test: +(.+) +\\*\\*\\)"
--| Define test case name
regexTagDesribe = "\\(\\*\\* *Describe: +(.+) +\\*\\*\\)"
--| Define thrown error
regexTagError = "\\(\\*\\* *Throws: +(.+) +\\*\\*\\)"
--| Skip this test if the value is "Yes"
regexTagSkip = "\\(\\*\\* *Skip: +(.+) +\\*\\*\\)"
--| Define test output
regexTagOutput = "\\(\\*\\* *Output: +(.+) +\\*\\*\\)"

--| Metadata about the test
data SourceMetadata = SourceMetadata
  { testDescription :: String, testName :: String, errorRegex :: Maybe String, shouldSkip :: Bool, expectedOutput :: String }

--| Replace all regex occurrences within the given string
replaceAll :: String -> String -> String -> String
replaceAll regex new_str str  =
    let parts = concatMap elems (str  =~  regex :: [MatchArray])
    in foldl (replace' new_str) str (reverse parts)

  where
     replace' :: [a] -> [a] -> (Int, Int) -> [a]
     replace' new list (shift, l)   =
        let (pre, post) = splitAt shift list
        in pre ++ new ++ drop l post

--| Removes all tags from the test content
removeAllTags :: String -> String
removeAllTags = replaceAll "\\(\\*\\* *(.+): +(.+) +\\*\\*\\)" ""

--| Extract tag value using the given regex
extractTagUsing :: String -> SourceMetadata -> String -> (SourceMetadata -> [String] -> SourceMetadata) -> IO SourceMetadata
extractTagUsing input meta regex foldFn = do
    (a, b, c, descrMatchGroups) <- return ((input =~ regex) :: (String, String, String, [String]))
    return $ foldl foldFn meta [descrMatchGroups]

--| Extract description from the test metadata
extractTagDescriptionFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagDescriptionFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testDescription = unpack $ strip $ pack h }

--| Extract name from the test metadata
extractTagTestFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagTestFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { testName = unpack $ strip $ pack h }

--| Extract error string from the test metadata
extractTagErrorFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagErrorFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { errorRegex = Just $ unpack $ strip $ pack h }

--| Extract skip flag from the test metadata
extractTagSkipFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagSkipFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { shouldSkip = unpack (strip $ pack h) == "Yes" }

--| Extract desired test output from the test metadata
extractTagOutputFn :: SourceMetadata -> [String] -> SourceMetadata
extractTagOutputFn meta matches = case matches of
    [] -> meta
    (h:_) -> meta { expectedOutput = unpack $ strip $ pack h }

--| Extract test metadata from the given test content
extractTestMetadata :: String -> IO SourceMetadata
extractTestMetadata input = do
    let initMeta = SourceMetadata { testDescription = "", testName = "", errorRegex = Nothing, shouldSkip = False, expectedOutput = "" }
    meta0 <- extractTagUsing input initMeta regexTagDesribe extractTagDescriptionFn
    meta1 <- extractTagUsing input meta0 regexTagTest extractTagTestFn
    meta2 <- extractTagUsing input meta1 regexTagError extractTagErrorFn
    meta3 <- extractTagUsing input meta2 regexTagSkip extractTagSkipFn
    extractTagUsing input meta3 regexTagOutput extractTagOutputFn
