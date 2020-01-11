module SSA.Syntax where

import           Syntax.Base

data SSAVar = SSAVarID Int
  deriving (Eq, Show)

data SSAOperation1 = OpNeg
  deriving (Eq, Show)
data SSAOperation2 = OpMod | OpDiv | OpSub | OpAdd
  deriving (Eq, Show)

data SSAExpression =
  Noop
  | Move SSAVar SSAVar
  | Expr2 SSAVar SSAOperation2 SSAVar SSAVar
  | Expr1 SSAVar SSAOperation1 SSAVar
  | Func SSAVar [SSAExpression]
  deriving (Eq, Show)