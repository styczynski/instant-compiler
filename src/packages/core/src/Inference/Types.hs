{-|
Module      : Inference.Types
Description : Types for inferencer
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides all base and complex types used by inferencer.
-}
module Inference.Types where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           Syntax.Base             hiding ( TV )

import qualified Data.Map                      as Map

-- | Free type variable
newtype TypeVar = TV String
  deriving (Show, Eq, Ord)

-- | Typing environment
data TypeEnvironment = TypeEnvironment { types :: Map.Map Ident Scheme }
  deriving (Eq, Show)

data TypeAnnotation = AnnotationEnv TypeEnvironment deriving (Show, Eq)

data TypeMeta = TypeMetaNone | TypeMeta Int deriving (Show, Eq)

-- | Data types for inference
data Type
  = TypeVar TypeMeta TypeVar
  | TypeStatic TypeMeta String
  | TypeArrow TypeMeta Type Type
  | TypeList TypeMeta Type
  | TypeTuple TypeMeta Type Type
  | TypeUnit TypeMeta
  | TypeComplex TypeMeta String [Type]
  | TypePoly TypeMeta [Type]
  | TypeAnnotated TypeMeta TypeAnnotation
  deriving (Show, Eq)

-- | Type scheme
data Scheme = Scheme [TypeVar] Type
  deriving (Show, Eq)

-- | Extracts free variables from the type
getTypeFVNames :: Type -> [String]
getTypeFVNames (TypeVar  _ (TV name)) = [name]
getTypeFVNames (TypeList _ t        ) = getTypeFVNames t
getTypeFVNames (TypeArrow _ a b     ) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames (TypePoly _ alternatives) =
  foldr (\t acc -> acc ++ (getTypeFVNames t)) [] alternatives
getTypeFVNames (TypeComplex _ name deps) =
  foldr (\t acc -> acc ++ (getTypeFVNames t)) [] deps
getTypeFVNames (TypeTuple _ a b) = (getTypeFVNames a) ++ (getTypeFVNames b)
getTypeFVNames _               = []

-- | Helper to reassign types for readability
remapTypesRec :: Map.Map String String -> Type -> Type
remapTypesRec fvMap t@(TypeVar r (TV name)) = case Map.lookup name fvMap of
  (Just newName) -> (TypeVar r (TV newName))
  (Nothing     ) -> t
remapTypesRec fvMap (TypeAnnotated r _) = TypeUnit r
remapTypesRec fvMap (TypeList      r t) = TypeList r $ remapTypesRec fvMap t
remapTypesRec fvMap (TypeArrow r a b) =
  TypeArrow r (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec fvMap (TypeComplex r name deps) =
  TypeComplex r name $ map (remapTypesRec fvMap) deps
remapTypesRec fvMap (TypePoly r alternatives) =
  TypePoly r $ map (remapTypesRec fvMap) alternatives
remapTypesRec fvMap (TypeTuple r a b) =
  TypeTuple r (remapTypesRec fvMap a) (remapTypesRec fvMap b)
remapTypesRec _ v = v

-- | Gets unique free variables names
fvUnique :: (Eq a) => [a] -> [a]
fvUnique []       = []
fvUnique (x : xs) = x : fvUnique (filter (/= x) xs)

-- | Generator for readable types (remapTypes)
typesLetters :: [String]
typesLetters = [1 ..] >>= flip replicateM ['a' .. 'z']

-- | Map types to fresh ones (for readability)
remapTypes :: Type -> Type
remapTypes t =
  let fvNames = fvUnique $ getTypeFVNames t
  in  let fvMap =
              foldr (\(l, name) acc -> Map.insert name ("'" ++ l) acc) Map.empty
                $ zip typesLetters fvNames
      in  remapTypesRec fvMap t

isNotPlaceholder :: Type -> Bool
isNotPlaceholder (TypeVar _ _) = False
isNotPlaceholder _           = True

-- | Helper to print types in readable format
typeToStrRec :: [TypeVar] -> Type -> [Type] -> String
typeToStrRec vars (TypeUnit _) funArgs = "Void"
typeToStrRec vars (TypeAnnotated _ (AnnotationEnv v)) funArgs =
  "export{" ++ (show v) ++ "}"
typeToStrRec vars (TypeList _ t) funArgs = "[" ++ (typeToStrRec vars t []) ++ "]"

--typeToStrRec vars (TypeArrow a b) funArgs =
--  "(" ++ (typeToStrRec vars a []) ++ ") -> " ++ (typeToStrRec vars b [])

typeToStrRec vars (TypeArrow _ a (TypeArrow r b c)) funArgs =
  typeToStrRec vars (TypeArrow r b c) (funArgs ++ [a])

typeToStrRec vars (TypeArrow _ a retType) funArgs =
  "("
      ++ (foldr
           (\el acc ->
             acc
               ++ (if length acc <= 0 then "" else ", ")
               ++ (typeToStrRec vars el [])
           )
           ""
           ([a] ++ funArgs)
         )
      ++ "): " ++ (typeToStrRec vars retType [])

typeToStrRec vars (TypeVar    _ (TV name)) funArgs = name
typeToStrRec vars (TypeStatic _ name     ) funArgs = name
typeToStrRec vars (TypePoly _ alternatives) funArgs =
  "[< "
    ++ (foldr
         (\el acc ->
           acc
             ++ (if length acc <= 0 then "" else "| ")
             ++ (typeToStrRec vars el [])
         )
         ""
         (filter isNotPlaceholder alternatives)
       )
    ++ "]"
typeToStrRec vars (TypeComplex _ name deps) funArgs =
  name
    ++ " ("
    ++ (foldr
         (\el acc ->
           acc
             ++ (if length acc <= 0 then "" else ", ")
             ++ (typeToStrRec vars el [])
         )
         ""
         deps
       )
    ++ ")"
typeToStrRec vars (TypeTuple _ (TypeUnit _) (TypeUnit _)) funArgs = "()"
typeToStrRec vars (TypeTuple _ a (TypeTuple _ (TypeUnit _) (TypeUnit _))) funArgs =
  typeToStrRec vars a []
typeToStrRec vars (TypeTuple _ a b) funArgs =
  (typeToStrRec vars a []) ++ " * " ++ (typeToStrRec vars b [])

-- | Print readable text representation for type
typeToStr :: [TypeVar] -> Type -> String
typeToStr l t = typeToStrRec l (remapTypes t) []

-- | Print readable text representation for type schema
schemeToStr :: Scheme -> String
schemeToStr (Scheme vars t) = typeToStr vars t

