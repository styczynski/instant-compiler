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

import           Syntax.Base

import           Inference.Types
import           Inference.TypingEnvironment

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