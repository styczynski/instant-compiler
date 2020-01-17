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

data TypeMeta = TypeMetaNone | TypeMeta [Int] deriving (Show, Eq)

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
  deriving Show

instance Eq Type where
  (TypeVar _ v1) == (TypeVar _ v2) = v1 == v2
  (TypeStatic _ s1) == (TypeStatic _ s2) = s1 == s2
  (TypeArrow _ ta1 tb1) == (TypeArrow _ ta2 tb2) = (ta1 == ta2) && (tb1 == tb2)
  (TypeList _ t1) == (TypeList _ t2) = t1 == t2
  (TypeTuple _ ta1 tb1) == (TypeTuple _ ta2 tb2) = (ta1 == ta2) && (tb1 == tb2)
  (TypeUnit _) == (TypeUnit _) = True
  (TypeComplex _ s1 tl1) == (TypeComplex _ s2 tl2) = (s1 == s2) && (tl1 == tl2)
  (TypePoly _ tl1) == (TypePoly _ tl2) = tl1 == tl2
  (TypeAnnotated _ a) == (TypeAnnotated _ b) = a == b
  _ == _ = False

getTypeMeta :: Type -> TypeMeta
getTypeMeta v = case v of
  TypeVar m _ -> m
  TypeStatic m _ -> m
  TypeArrow m _ _ -> m
  TypeList m _ -> m
  TypeTuple m _ _ -> m
  TypeUnit m -> m
  TypeComplex m _ _ -> m
  TypePoly m _ -> m
  TypeAnnotated m _ -> m

joinMetaEx :: Bool -> TypeMeta -> TypeMeta -> TypeMeta
joinMetaEx _ TypeMetaNone x = x
joinMetaEx mode x TypeMetaNone = if mode then TypeMetaNone else x
joinMetaEx mode (TypeMeta a) v@(TypeMeta b) = if mode then v else TypeMeta $ a ++ b
joinMetaEx mode _ m = if mode then m else TypeMetaNone

joinMeta :: TypeMeta -> TypeMeta -> TypeMeta
joinMeta = joinMetaEx False

withMetaEx :: Bool -> Type -> TypeMeta -> Type
withMetaEx mode v m = case v of
  TypeVar m0 a -> TypeVar (joinMetaEx mode m0 m) a
  TypeStatic m0 a -> TypeStatic (joinMetaEx mode m0 m) a
  TypeArrow m0 a b -> TypeArrow (joinMetaEx mode m0 m) a b
  TypeList m0 a -> TypeList (joinMetaEx mode m0 m) a
  TypeTuple m0 a b -> TypeTuple (joinMetaEx mode m0 m) a b
  TypeUnit m0 -> TypeUnit (joinMetaEx mode m0 m)
  TypeComplex m0 a b -> TypeComplex (joinMetaEx mode m0 m) a b
  TypePoly m0 a -> TypePoly (joinMetaEx mode m0 m) a
  TypeAnnotated m0 a -> TypeAnnotated (joinMetaEx mode m0 m) a

withMeta :: Type -> TypeMeta -> Type
withMeta = withMetaEx False

clearMeta :: Type -> Type
clearMeta v = withMetaEx True v TypeMetaNone

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

