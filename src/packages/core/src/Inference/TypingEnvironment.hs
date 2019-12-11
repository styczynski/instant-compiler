{-|
Module      : Inference.TypingEnvironment
Description : All base definitions for inference
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides all base data types used by inference modules i.e.
  substitution mappings, typing environment, base typechecking errors etc.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Inference.TypingEnvironment where

import           Inference.Types

import           Syntax.Base             hiding ( TypeConstraint )

import           Prelude                 hiding ( lookup )

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable           hiding ( toList )
import qualified Data.Map                      as Map

-- | Substitution mapping
newtype Substitution a b = Subst (Map.Map a b) deriving (Eq, Show)
type TypeSubstitution = Substitution TypeVar Type

-- | Empty subsitution map that subsitutes nothing
emptySubst :: TypeSubstitution
emptySubst = Subst $ Map.empty

-- | Traceable
class (Show t) => Traceable t where
  toString :: t -> String

instance Traceable String where
  toString s = s

-- | Payload for typechecking errors
data (Traceable t) => TypeErrorPayload t = EmptyPayload | TypeErrorPayload t deriving (Show)

-- | Type constraints with error payload annotation and unifier for types
data (Traceable t) => TypeConstraint t = TypeConstraint (TypeErrorPayload t) (Type, Type) deriving (Show)
data (Traceable t) => TypeUnifier t = NoUnifier | TypeUnifier [TypeConstraint t] TypeSubstitution

-- | Base inference monad and state
type Infer t
  = StateT (InferState t) (ReaderT (TypeEnvironment) (ExceptT (TypeError t) IO))
data (Traceable t) => InferState t = InferState {
  count :: Int,
  tCount :: Int,
  tagMap :: Map.Map String Int,
  inferTrace :: [TypeErrorPayload t],
  lastInferExpr :: TypeErrorPayload t
}

-- | Base typechecking errors
data (Traceable t) => TypeError t
  = UnificationFail (TypeErrorPayload t) Type Type
  | InfiniteType (TypeErrorPayload t) TypeVar Type
  | UnboundVariable (TypeErrorPayload t) Ident
  | Ambigious (TypeErrorPayload t) [TypeConstraint t]
  | UnificationMismatch (TypeErrorPayload t) [Type] [Type]
  | Debug (TypeErrorPayload t) String
  deriving (Show)

-- | Empty inference state
initInfer :: (Traceable t) => InferState t
initInfer = InferState { count         = 0
                       , tCount        = 0
                       , tagMap        = Map.empty
                       , inferTrace    = []
                       , lastInferExpr = EmptyPayload
                       }

-- | Stringify type contraint (for debug purposes)
constraintToStr :: (Traceable t) => TypeConstraint t -> String
constraintToStr (TypeConstraint _ (a, b)) =
  (typeToStr [] a) ++ " ~ " ++ (typeToStr [] b)

-- | Stringify type contraints list (for debug purposes)
constraintsListToStr :: (Traceable t) => [TypeConstraint t] -> String
constraintsListToStr l =
  "{"
    ++ (foldr
         (\t acc ->
           acc
             ++ (if (length acc) <= 0 then "" else ", ")
             ++ (constraintToStr t)
         )
         ""
         l
       )
    ++ "}"

-- | Empty typing environment
empty :: TypeEnvironment
empty = TypeEnvironment Map.empty

-- | Create identificator inside the environment
(++>) :: TypeEnvironment -> (Ident, Scheme) -> TypeEnvironment
(++>) env (name, scheme) =
  let newEnv = env { types = Map.insert name scheme (types env) } in newEnv

-- | Remvoe identificator from the environment
(-->) :: TypeEnvironment -> Ident -> TypeEnvironment
(-->) (TypeEnvironment env) name = TypeEnvironment (Map.delete name env)

-- | Execute monad in shadowed environment
(==>) :: (Ident, Scheme) -> Infer t a -> Infer t a
(==>) (x, sc) m = local (\env -> (env --> x) ++> (x, sc)) m

-- | Lookup typing environment for the identificator
(??) :: TypeEnvironment -> Ident -> Maybe Scheme
(??) (TypeEnvironment env) name = Map.lookup name env
