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

import           Inference.Syntax
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable           hiding ( toList )
import qualified Data.Map                      as Map

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
