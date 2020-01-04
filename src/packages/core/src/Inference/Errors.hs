{-|
Module      : Inference.Errors
Description : Typechecking errors utilities
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This file contains utilites for handling typechecking errors.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Inference.Errors where

import           Syntax.Base             hiding ( TV )

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types

import qualified Data.Map                      as Map
import           Data.Text.Internal.Search
import qualified          Text.Fuzzy as Fuzzy
import qualified Data.Text                     as T
import Data.List
import Data.Char (isSpace)

-- | Translates types list into readable human representation
typesListToStr :: [Type] -> String
typesListToStr l =
  "{"
    ++ (foldr
         (\t acc ->
           acc ++ (if (length acc) <= 0 then "" else ", ") ++ (typeToStr [] t)
         )
         ""
         l
       )
    ++ "}"

-- | TODO: Document function
unstackErrors :: (AST r t) => (r, t) -> [(TypeErrorPayload t)] -> [(TypeErrorPayload t)]
unstackErrors _ payl =  foldl (\acc el -> case el of
  EmptyPayload -> if (length acc) > 0 then init acc else []
  v -> acc ++ [v]) [] payl

-- | TODO: Document function
mapErrors :: (AST r t) => (r, t) -> [(TypeErrorPayload t)] -> [t]
mapErrors s0@(r0, _) payl =
  foldl (\acc el -> case el of
    (TypeErrorPayload t) -> acc ++ [t]
    _ -> acc ++ [(getEmptyPayload r0)]) [] (unstackErrors s0 payl)

-- | Creates readeable human message from the error payload
generateTypePayloadMessage :: (AST r t) => (r, t) -> [(TypeErrorPayload t)] -> String
generateTypePayloadMessage _ [] =
  "Typechecking error:\nLocation: <unknown>\n\n"
generateTypePayloadMessage (r0, t0) payl =
  --"Typechecking error:\nLocation: " ++ ast ++ "\n\n"
  "Typechecking error:\nLocation: " ++ (describeErrors r0 $ mapErrors (r0, t0) payl) ++ "\n\n"

rstrip = reverse . dropWhile isSpace . reverse

prettifyErrorMessage :: Bool -> String -> String
prettifyErrorMessage oneLine str =
  foldl (\acc el -> if (length acc > 0) then acc ++ "\n     |        " ++ el else el) "" $ filter (not . null) $ lines $ if oneLine then intercalate " " $ lines str else str

unwrapMaybe :: Maybe t -> t
unwrapMaybe (Just a) = a

getMostSimilarTypeNames :: TypeEnvironment -> String -> [(String, Scheme)]
getMostSimilarTypeNames typeEnv name =
  map (\f -> (Fuzzy.rendered f, (unwrapMaybe $ Map.lookup (Ident $ Fuzzy.original f) (types typeEnv))) ) $ Fuzzy.filter name (map (\(Ident name) -> name) $ Map.keys $ types typeEnv) "" "" (\t -> t) False


-- | Translates typing error into readable string
typeErrorToStr :: (AST r t) => (r,t) -> TypeError r t -> IO String
typeErrorToStr s0 (UnificationFail payl a b) =
    formatErrorDetails s0 (mapErrors s0 payl) "Mismatched types." $ "Cannot match types, expected: "
    ++ (typeToStr [] b)
    ++ ", got: "
    ++ (typeToStr [] a)
typeErrorToStr s0 (Debug payl mes) = formatErrorDetails s0 (mapErrors s0 payl) "Debug message." $ mes
typeErrorToStr s0 (UnificationMismatch payl a b) =
  formatErrorDetails s0 (mapErrors s0 payl) "Mismatched types." $ "Cannot match types, mismatch when unyfying: "
    ++ (typesListToStr a)
    ++ " and "
    ++ (typesListToStr b)
typeErrorToStr s0 (Ambigious payl a) =
  formatErrorDetails s0 (mapErrors s0 payl) "Types cannot be determined." $ "Cannot infer types, expression is ambigious: "
    ++ (constraintsListToStr a)
typeErrorToStr s0 (UnboundVariable payl (Ident a) typeEnv) = do
  suggestions <- return $ getMostSimilarTypeNames typeEnv a
  suggestionsText <- return $ foldr (\(name, scheme) acc -> let elText = "      - Variable: " ++ name ++ " of type " ++ (schemeToStr scheme) in if (length acc) == 0 then elText else acc ++ "\n" ++ elText ) "" (take 3 suggestions)
  suggestionsText <- return $ if (length suggestionsText) == 0 then "      No suggestions to display." else suggestionsText
  formatErrorDetails s0 (mapErrors s0 payl) "Invalid identifier was used." $  "Variable not in scope: \"" ++ a ++ "\"\n     Maybe you made a typo? Suggested alternatives are:\n" ++ suggestionsText ++ "\n"
typeErrorToStr s0 (InfiniteType payl (TV v) t) =
  formatErrorDetails s0 (mapErrors s0 payl) "Invalid type was specified." $ "Infinite type detected: "
    ++ v
    ++ "': "
    ++ (typeToStr [] t)
typeErrorToStr s0 e =
  -- (generateTypePayloadMessage EmptyPayload)
  formatErrorDetails s0 [] "Unknown error has occured." $  "Typechecking error:\nLocation: <unknown>\n\n"
    ++ "Got unexpected error during type inference phase.\n"
  --  ++ (show e)

formatErrorDetails :: (AST r t) => (r,t) -> [t] -> String -> String -> IO String
formatErrorDetails (r0,_) trace message errorText =  do
  traceStr <- return
    $ foldl (\acc el -> acc ++ "     | Infer: " ++ (describeTraceItem r0 el) ++ "\n") "" trace
  --traceStr <- return "<no_trace>"
  trace <- return $ foldl (\x y -> y:x) [] trace
  traceContextGlobalString <- return $ describeErrors r0 [trace !! 2]
  traceContextLocalString <- return $ describeErrors r0 [trace !! 0]
  case indices (T.pack traceContextLocalString) (T.pack traceContextGlobalString) of
    (fInd : _) ->
      let pointerText = (T.unpack (T.replicate fInd (T.pack " "))) ++ "^"
      in  return $ message
          ++ "\n   "
          ++ traceContextGlobalString
          ++ "\n   "
          ++ pointerText
          ++ "\n    "
          ++ errorText
          ++ "\n"
          ++ traceStr
    _ -> return $ message
      ++ "\n   "
      ++ traceContextGlobalString
      ++ "in [" ++ traceContextLocalString ++ "]"
      ++ "    "
      ++ errorText
      ++ "\n"
      ++ traceStr