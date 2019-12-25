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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

instance AST Program String where
  getEmptyPayload _ = ""
  getPayload a _ = (Program [], show a)
  toString p = show p
  simplify p = simplifyProgram p

getPreface :: (AST r t) => (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
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

simplifyProgram :: (AST r t) => Program -> Infer r t (SimplifiedExpr r t)
simplifyProgram (Program defs) = do
  u <- valueOfType "Unit"
  p0 <- foldrM (\def acc -> simplifyTopDef def acc) (SimplifiedCall (SimplifiedVariable $ Ident "main") u) defs
  p <- getPreface p0
  --_ <- liftIO $ putStrLn $ show p
  return p

simplifyTopDef :: (AST r t) => TopDef -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
simplifyTopDef ast@(FnDef retType name args body) expr = do
  markTrace expr ast
  b <- simplifyBlock body expr
  l <- createLambda (map (\(Arg typeName (Ident argName)) -> (getTypeName typeName, argName)) args) (getTypeName retType) b
  tl <- return $ SimplifiedLet name l expr
  _ <- liftIO $ putStrLn $ show tl
  r <- addExprAnnot $ return tl
  unmarkTrace expr ast
  return r

simplifyBlock :: (AST r t) => Block -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
simplifyBlock ast@(Block statements) expr = do
  markTrace expr ast
  u <- valueOfType "Unit"
  r <- foldrM (\stmt acc -> simplifyStatement acc stmt) u statements
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r

simplifyStatement :: (AST r t) => (SimplifiedExpr r t) -> Stmt -> Infer r t (SimplifiedExpr r t)
simplifyStatement expr (Decl typeName inits) = do
  foldM (simplifyStatementDecl typeName) expr inits
simplifyStatement expr (SExp stmtExpr) = do
  e <- simplifyExpr stmtExpr expr
  return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) e) expr
simplifyStatement _ VRet = valueOfType "Unit"
simplifyStatement expr (Ret retExpr) = do
  e <- simplifyExpr retExpr expr
  return e

getTypeDefaultValue :: (AST r t) => TypeName -> (SimplifiedExpr r t)
getTypeDefaultValue Int = SimplifiedConstInt 0
getTypeDefaultValue Str = SimplifiedConstString ""
getTypeDefaultValue Bool = SimplifiedConstBool False

getTypeName :: TypeName -> String
getTypeName Int = "Int"
getTypeName Bool = "Bool"
getTypeName Str = "String"
getTypeName Void = "Unit"

getTypeDefaultAnnot :: (AST r t) => TypeName -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
getTypeDefaultAnnot typeName e = checkType (getTypeName typeName) e

simplifyStatementDecl :: (AST r t) => TypeName -> (SimplifiedExpr r t) -> Item -> Infer r t (SimplifiedExpr r t)
simplifyStatementDecl typeName expr ast@(NoInit name) = do
  markTrace expr ast
  r <- return $ SimplifiedLet name (getTypeDefaultValue typeName) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatementDecl typeName expr ast@(Init name initExpr) = do
  markTrace expr ast
  e <- simplifyExpr initExpr expr
  annotExp <- getTypeDefaultAnnot typeName e
  r <- return $ SimplifiedLet name annotExp expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
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

getAppArgs :: (AST r t) => [(SimplifiedExpr r t)] -> Infer r t [(SimplifiedExpr r t)]
getAppArgs [] = do
  u <- valueOfType "Unit"
  return [u]
getAppArgs args = return args

simplifyExpr :: (AST r t) => Expr -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
simplifyExpr e expr = do
  markTrace expr e
  r <- simplifyExpr_ e expr
  r <- addExprAnnot $ return r
  unmarkTrace expr e
  return r


simplifyExpr_ :: (AST r t) => Expr -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
simplifyExpr_ (EVar name) _ = return $ SimplifiedVariable name
simplifyExpr_ (ELitInt val) _ = return $ SimplifiedConstInt val
simplifyExpr_ (EString val) _ = return $ SimplifiedConstString val
simplifyExpr_ ELitTrue _ = return $ SimplifiedConstBool True
simplifyExpr_ ELitFalse _ = return $ SimplifiedConstBool False
simplifyExpr_ (Neg expr) e = do
  e <- simplifyExpr expr e
  createNameCall "neg" [e]
simplifyExpr_ (Not expr) e = do
  e <- simplifyExpr expr e
  createNameCall "not" [e]
simplifyExpr_ (EMul expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getMulOpName op) [e1, e2]
simplifyExpr_ (ERel expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getRelOpName op) [e1, e2]
simplifyExpr_ (EAdd expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getAddOpName op) [e1, e2]
simplifyExpr_ (EApp (Ident name) exprs) e = do
  exprsSimpl <- mapM (\expr -> simplifyExpr expr e) exprs
  args <- getAppArgs exprsSimpl
  createNameCall name args