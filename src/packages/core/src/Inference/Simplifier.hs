{-|
Module      : Inference.Simplifier
Description : AST Simplifier
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module exprorts simplifiers that translate base AST into simplified representation.
  The AST types for simplified syntax are available in Inference.Syntax module.

  The simplified syntax allows only very basic constructions in addition to
  type assertions, dummy values (that do not exist, but have their exact type) and more
  contructions that make type inference easier to implement.
-}
module Inference.Simplifier where

import           Syntax.Base

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.InferencerUtils

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Function that infers type for given expression
type InferenceFn = TypeExpression -> Infer Scheme

-- | Translate AST operation node to readable operation name
getOperatorName :: OperatorAny -> String
getOperatorName (OperatorAnyA  (OperatorA name)) = name
getOperatorName (OperatorAnyB  (OperatorB name)) = name
getOperatorName (OperatorAnyC  (OperatorC name)) = name
getOperatorName (OperatorAnyD  (OperatorD name)) = name
getOperatorName (OperatorAnyDS (OperatorDS    )) = "*"
getOperatorName (OperatorAnyE  (OperatorE name)) = name
getOperatorName (OperatorAnyF  (OperatorF name)) = name

------------------------------------------------------------------
--        Simplification for various types of AST nodes         --
------------------------------------------------------------------

-- empty --
