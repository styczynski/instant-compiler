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
import           Data.List as L

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.InferencerUtils
import           Inference.ASTUtils

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

------------------------------------------------------------------
--        Simplification for various types of AST nodes         --
------------------------------------------------------------------

getPreface :: SimplifiedExpr -> Infer SimplifiedExpr
getPreface p = do
  declareTypes [
    ("then", "'a -> 'b -> 'b"),
    ("not", "Bool -> Bool"),
    ("neg", "Int -> Int"),
    ("+", "Int -> Int -> Int"),
    ("-", "Int -> Int -> Int"),
    ("/", "Int -> Int -> Int"),
    ("*", "Int -> Int -> Int"),
    ("=", "'a -> 'a -> Bool"),
    ("<=", "'a -> 'a -> Bool"),
    (">=", "'a -> 'a -> Bool"),
    ("==", "'a -> 'a -> Bool"),
    ("!=", "'a -> 'a -> Bool")] p

simplifyProgram :: Program -> Infer SimplifiedExpr
simplifyProgram (Program defs) = do
  u <- valueOfType "Unit"
  p0 <- foldrM (\def acc -> simplifyTopDef def acc) (SimplifiedCall (SimplifiedVariable $ Ident "main") u) defs
  p <- getPreface p0
  --_ <- liftIO $ putStrLn $ show p
  return p

simplifyTopDef :: TopDef -> SimplifiedExpr -> Infer SimplifiedExpr
simplifyTopDef ast@(FnDef retType name args body) expr = do
  markTrace ast
  b <- simplifyBlock body
  l <- createLambda (map (\(Arg typeName (Ident argName)) -> (getTypeName typeName, argName)) args) (getTypeName retType) b
  tl <- return $ SimplifiedLet name l expr
  _ <- liftIO $ putStrLn $ show tl
  r <- addExprAnnot $ return tl
  unmarkTrace ast
  return r

simplifyBlock :: Block -> Infer SimplifiedExpr
simplifyBlock ast@(Block statements) = do
  markTrace ast
  u <- valueOfType "Unit"
  r <- foldrM (\stmt acc -> simplifyStatement acc stmt) u statements
  r <- addExprAnnot $ return r
  unmarkTrace ast
  return r

simplifyStatement :: SimplifiedExpr -> Stmt -> Infer SimplifiedExpr
simplifyStatement expr (Decl typeName inits) = do
  foldM (simplifyStatementDecl typeName) expr inits
simplifyStatement expr (SExp stmtExpr) = do
  e <- simplifyExpr stmtExpr
  return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) e) expr
simplifyStatement _ VRet = valueOfType "Unit"
simplifyStatement _ (Ret retExpr) = do
  e <- simplifyExpr retExpr
  return e

getTypeDefaultValue :: TypeName -> SimplifiedExpr
getTypeDefaultValue Int = SimplifiedConstInt 0
getTypeDefaultValue Str = SimplifiedConstString ""
getTypeDefaultValue Bool = SimplifiedConstBool False

getTypeName :: TypeName -> String
getTypeName Int = "Int"
getTypeName Bool = "Bool"
getTypeName Str = "String"
getTypeName Void = "Unit"

getTypeDefaultAnnot :: TypeName -> SimplifiedExpr -> Infer SimplifiedExpr
getTypeDefaultAnnot typeName e = checkType (getTypeName typeName) e

simplifyStatementDecl :: TypeName -> SimplifiedExpr -> Item -> Infer SimplifiedExpr
simplifyStatementDecl typeName expr ast@(NoInit name) = do
  markTrace ast
  r <- return $ SimplifiedLet name (getTypeDefaultValue typeName) expr
  r <- addExprAnnot $ return r
  unmarkTrace ast
  return r
simplifyStatementDecl typeName expr ast@(Init name initExpr) = do
  markTrace ast
  e <- simplifyExpr initExpr
  annotExp <- getTypeDefaultAnnot typeName e
  r <- return $ SimplifiedLet name annotExp expr
  r <- addExprAnnot $ return r
  unmarkTrace ast
  return r


getMulOpName :: MulOp -> String
getMulOpName Times = "*"
getMulOpName Div = "/"
getMulOpName Mod = "%"

getAddOpName :: AddOp -> String
getAddOpName Plus = "+"
getAddOpName Minus = "-"

getRelOpName :: RelOp -> String
getRelOpName LTH = "<"
getRelOpName LE = "<="
getRelOpName GTH = ">"
getRelOpName GE = ">="
getRelOpName EQU = "=="
getRelOpName NE = "!="

getAppArgs :: [SimplifiedExpr] -> Infer [SimplifiedExpr]
getAppArgs [] = do
  u <- valueOfType "Unit"
  return [u]
getAppArgs args = return args

simplifyExpr :: Expr -> Infer SimplifiedExpr
simplifyExpr e = do
  markTrace e
  r <- simplifyExpr_ e
  r <- addExprAnnot $ return r
  unmarkTrace e
  return r


simplifyExpr_ :: Expr -> Infer SimplifiedExpr
simplifyExpr_ (EVar name) = return $ SimplifiedVariable name
simplifyExpr_ (ELitInt val) = return $ SimplifiedConstInt val
simplifyExpr_ (EString val) = return $ SimplifiedConstString val
simplifyExpr_ ELitTrue = return $ SimplifiedConstBool True
simplifyExpr_ ELitFalse = return $ SimplifiedConstBool False
simplifyExpr_ (Neg expr) = do
  e <- simplifyExpr expr
  createNameCall "neg" [e]
simplifyExpr_ (Not expr) = do
  e <- simplifyExpr expr
  createNameCall "not" [e]
simplifyExpr_ (EMul expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getMulOpName op) [e1, e2]
simplifyExpr_ (ERel expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getRelOpName op) [e1, e2]
simplifyExpr_ (EAdd expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getAddOpName op) [e1, e2]
simplifyExpr_ (EApp (Ident name) exprs) = do
  exprsSimpl <- mapM simplifyExpr exprs
  args <- getAppArgs exprsSimpl
  createNameCall name args