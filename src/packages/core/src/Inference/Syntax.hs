{-|
Module      : Inference.Syntax
Description : Simplified AST structures
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This code introduces simplified AST structure.

  The simplified AST is produced from normal program AST by Inference.Simplifier.
  It allows only very basic constructions in addition to
  type assertions, dummy values (that do not exist, but have their exact type) and more
  constructions that make type inference easier to implement.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Inference.Syntax where

import           Syntax.Base hiding ( TypeConstraint )
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import qualified Data.Map                      as Map

import           Inference.Types
--import           Inference.TypingEnvironment

-- | Substitution mapping
newtype Substitution a b = Subst (Map.Map a b) deriving (Eq, Show)
type TypeSubstitution = Substitution TypeVar Type

-- | Empty subsitution map that subsitutes nothing
emptySubst :: TypeSubstitution
emptySubst = Subst $ Map.empty

-- | AST class
class (Show t, Show r) => AST r t | r -> t where
  getEmptyPayload :: r -> t
  getPayload :: (Print a, Show a) => a -> (SimplifiedExpr r t) -> (r, t)
  describeErrors :: r -> [t] -> String
  describeTraceItem :: r -> t -> String
  simplify :: r -> Infer r t (SimplifiedExpr r t)
  describeErrorPosition :: r -> [t] -> (Int, Int, Int)

-- | Payload for typechecking errors
data TypeErrorPayload t = EmptyPayload | TypeErrorPayload t deriving (Show, Eq)

-- | Type constraints with error payload annotation and unifier for types
data (AST r t) => TypeConstraint r t = TypeConstraint [TypeErrorPayload t] (Type, Type) deriving (Show)
data (AST r t) => TypeUnifier r t = NoUnifier | TypeUnifier [TypeConstraint r t] TypeSubstitution
-- | Base inference monad and state
type Infer r t
  = StateT (InferState r t) (ReaderT (TypeEnvironment) (ExceptT (TypeError r t) IO))
data (AST r t) => InferState r t = InferState {
  count :: Int,
  tCount :: Int,
  tagMap :: Map.Map String Int,
  inferTrace :: [TypeErrorPayload t],
  root :: r
} deriving (Show)

-- | Base typechecking errors
data (AST r t) => TypeError r t
  = UnificationFail [TypeErrorPayload t] Type Type
  | InfiniteType [TypeErrorPayload t] TypeVar Type
  | UnboundVariable [TypeErrorPayload t] Ident TypeEnvironment
  | Ambigious [TypeErrorPayload t] [TypeConstraint r t]
  | UnificationMismatch [TypeErrorPayload t] [Type] [Type]
  | Debug [TypeErrorPayload t] String
  deriving (Show)

-- | Simplified AST representation
data (AST r t) => SimplifiedExpr r t
  = SimplifiedVariable Ident
  | SimplifiedCall (SimplifiedExpr r t) (SimplifiedExpr r t)
  | SimplifiedFunction Ident (SimplifiedExpr r t)
  | SimplifiedLet Ident TypeMeta (SimplifiedExpr r t) (SimplifiedExpr r t)
  | SimplifiedLetAs Ident TypeMeta (SimplifiedExpr r t) (SimplifiedExpr r t) (SimplifiedExpr r t)
  | SimplifiedIf (SimplifiedExpr r t) (SimplifiedExpr r t) (SimplifiedExpr r t)
  | SimplifiedFixPoint (SimplifiedExpr r t)
  | SimplifiedBinaryOp BinaryOp (SimplifiedExpr r t) (SimplifiedExpr r t)
  | SimplifiedUnaryOp UnaryOp (SimplifiedExpr r t)
  | SimplifiedSkip
  | SimplifiedCheck (SimplifiedExpr r t) Scheme
  | SimplifiedExportEnv
  | SimplifiedTyped Scheme
  | SimplifiedAnnotated [TypeErrorPayload t] (SimplifiedExpr r t)
  | SimplifiedConstBool Bool
  | SimplifiedConstInt Integer
  | SimplifiedConstString String
  | SimplifiedTag Ident (SimplifiedExpr r t)
  | SimplifiedTagUnpack Ident (SimplifiedExpr r t)
  | SimplifiedTagUnpackNonStrict Ident (SimplifiedExpr r t)
  | SimplifiedAlternatives [(SimplifiedExpr r t)]
  deriving (Show, Eq)

-- | Binary operation
data BinaryOp = OpSemicolon | OpSame | OpCustom String | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

-- | Unary operation
data UnaryOp = OpCustomUni String | OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int | OpListNth
  deriving (Eq, Ord, Show)