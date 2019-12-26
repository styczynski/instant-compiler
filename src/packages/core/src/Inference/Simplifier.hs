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

instance AST Program ASTNode where
  getEmptyPayload _ = ASTNone
  simplify p = simplifyProgram p
  describeErrors r payl = intercalate "" $ map (\p -> describeError r p) payl
  describeTraceItem r (ASTProgram ast) = "Program entry"
  describeTraceItem r (ASTTopDef ast@(FnDef typ (Ident name) arg _)) = "Top definition for \"" ++ name ++ "\": " ++ (prettifyErrorMessage True $ printTree $ FnDef typ (Ident name) arg (Block []))
  describeTraceItem r (ASTTypeName ast) = "Type name: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTStmt ast) = "Block statement: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTBlock ast) = "Block of code"
  describeTraceItem r (ASTItem ast) = "Item definition: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTExpr ast) = "Expression: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r _ = "Unknown AST part"

describeError :: Program -> ASTNode -> String
describeError r (ASTProgram ast) = printTree ast
describeError r (ASTExpr ast) = printTree ast
describeError r (ASTStmt p) = printTree p
describeError r (ASTBlock p) = printTree p
describeError r (ASTTopDef p) = printTree p
describeError r (ASTTypeName p) = printTree p
describeError r (ASTItem p) = printTree p
describeError r (ASTNone) = "<none>"


getPreface :: (SimplifiedExpr Program ASTNode) -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
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

simplifyProgram :: Program -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyProgram (Program defs) = do
  u <- valueOfType "Unit"
  p0 <- foldrM (\def acc -> simplifyTopDef def acc) (SimplifiedCall (SimplifiedVariable $ Ident "main") u) defs
  p <- getPreface p0
  --_ <- liftIO $ putStrLn $ show p
  return p

simplifyTopDef :: TopDef -> (SimplifiedExpr Program ASTNode) -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyTopDef ast@(FnDef retType name args body) expr = do
  markTrace expr $ ASTTopDef ast
  b <- simplifyBlock body expr
  l <- createLambda (map (\(Arg typeName (Ident argName)) -> (getTypeName typeName, argName)) args) (getTypeName retType) b
  tl <- return $ SimplifiedLet name l expr
  -- _ <- liftIO $ putStrLn $ show tl
  r <- addExprAnnot $ return tl
  unmarkTrace expr ast
  return r

simplifyBlock :: Block -> (SimplifiedExpr Program ASTNode) -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyBlock ast@(Block statements) expr = do
  markTrace expr $ ASTBlock ast
  u <- valueOfType "Unit"
  r <- foldrM (\stmt acc -> simplifyStatement acc stmt) u statements
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r

simplifyStatement :: (SimplifiedExpr Program ASTNode) -> Stmt -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyStatement expr ast@(Decl typeName inits) = do
  markTrace expr $ ASTStmt ast
  r <- foldM (simplifyStatementDecl typeName) expr inits
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(SExp stmtExpr) = do
  markTrace expr $ ASTStmt ast
  e <- simplifyExpr stmtExpr expr
  r <- return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) e) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@VRet = do
  markTrace expr $ ASTStmt ast
  r <- valueOfType "Unit"
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(Ret retExpr) = do
  markTrace expr $ ASTStmt ast
  r <- simplifyExpr retExpr expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r

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

simplifyStatementDecl :: TypeName -> (SimplifiedExpr Program ASTNode) -> Item -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyStatementDecl typeName expr ast@(NoInit name) = do
  markTrace expr $ ASTItem ast
  r <- return $ SimplifiedLet name (getTypeDefaultValue typeName) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatementDecl typeName expr ast@(Init name initExpr) = do
  markTrace expr $ ASTItem ast
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

simplifyExpr :: Expr -> (SimplifiedExpr Program ASTNode) -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
simplifyExpr e expr = do
  markTrace expr $ ASTExpr e
  r <- simplifyExpr_ e expr
  r <- addExprAnnot $ return r
  unmarkTrace expr e
  return r


simplifyExpr_ :: Expr -> (SimplifiedExpr Program ASTNode) -> Infer Program ASTNode (SimplifiedExpr Program ASTNode)
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