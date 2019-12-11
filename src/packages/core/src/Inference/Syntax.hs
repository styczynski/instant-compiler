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

-- | Traceable
class (Show t) => Traceable t where
  toString :: t -> String
  simplify :: t -> Infer t (SimplifiedExpr t)

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

-- | Simplified AST representation
data (Traceable t) => SimplifiedExpr t
  = SimplifiedVariable Ident
  | SimplifiedCall (SimplifiedExpr t) (SimplifiedExpr t)
  | SimplifiedFunction Ident (SimplifiedExpr t)
  | SimplifiedLet Ident (SimplifiedExpr t) (SimplifiedExpr t)
  | SimplifiedLetAs Ident (SimplifiedExpr t) (SimplifiedExpr t) (SimplifiedExpr t)
  | SimplifiedIf (SimplifiedExpr t) (SimplifiedExpr t) (SimplifiedExpr t)
  | SimplifiedFixPoint (SimplifiedExpr t)
  | SimplifiedBinaryOp BinaryOp (SimplifiedExpr t) (SimplifiedExpr t)
  | SimplifiedUnaryOp UnaryOp (SimplifiedExpr t)
  | SimplifiedSkip
  | SimplifiedCheck (SimplifiedExpr t) Scheme
  | SimplifiedExportEnv
  | SimplifiedTyped Scheme
  | SimplifiedAnnotated t (SimplifiedExpr t)
  | SimplifiedConstBool Bool
  | SimplifiedConstInt Integer
  | SimplifiedConstString String
  | SimplifiedTag Ident (SimplifiedExpr t)
  | SimplifiedTagUnpack Ident (SimplifiedExpr t)
  | SimplifiedTagUnpackNonStrict Ident (SimplifiedExpr t)
  | SimplifiedAlternatives [(SimplifiedExpr t)]
  deriving (Show, Eq)

-- | Binary operation
data BinaryOp = OpSemicolon | OpSame | OpCustom String | OpCons | OpTupleCons
  deriving (Eq, Ord, Show)

-- | Unary operation
data UnaryOp = OpCustomUni String | OpHead | OpTails | OpEmptyList | OpEmptyTuple | OpTupleNth Int Int | OpListNth
  deriving (Eq, Ord, Show)