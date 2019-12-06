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
  p0 <- simplifyTopDef $ head defs
  p <- getPreface p0
  _ <- liftIO $ putStrLn $ show p
  return p

simplifyTopDef :: TopDef -> Infer SimplifiedExpr
simplifyTopDef (FnDef retType name args body) = do
  b <- simplifyBlock body
  createLambda (map (\(Arg typeName (Ident argName)) -> (getTypeName typeName, argName)) args) (getTypeName retType) b

simplifyBlock :: Block -> Infer SimplifiedExpr
simplifyBlock (Block statements) = do
  foldrM (\stmt acc -> simplifyStatement acc stmt) SimplifiedSkip statements

simplifyStatement :: SimplifiedExpr -> Stmt -> Infer SimplifiedExpr
simplifyStatement expr (Decl typeName inits) = do
  foldM (simplifyStatementDecl typeName) expr inits
simplifyStatement expr (SExp stmtExpr) = do
  e <- simplifyExpr stmtExpr
  return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) expr) e
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

getTypeDefaultAnnot :: TypeName -> SimplifiedExpr -> Infer SimplifiedExpr
getTypeDefaultAnnot typeName e = return $ SimplifiedAnnotated (getTypeName typeName) e

simplifyStatementDecl :: TypeName -> SimplifiedExpr -> Item -> Infer SimplifiedExpr
simplifyStatementDecl typeName expr (NoInit name) = do
  return $ SimplifiedLet name (getTypeDefaultValue typeName) expr
simplifyStatementDecl typeName expr (Init name initExpr) = do
  e <- simplifyExpr initExpr
  annotExp <- getTypeDefaultAnnot typeName e
  return $ SimplifiedLet name annotExp expr

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

simplifyExpr :: Expr -> Infer SimplifiedExpr
simplifyExpr (EVar name) = return $ SimplifiedVariable name
simplifyExpr (ELitInt val) = return $ SimplifiedConstInt val
simplifyExpr (EString val) = return $ SimplifiedConstString val
simplifyExpr ELitTrue = return $ SimplifiedConstBool True
simplifyExpr ELitFalse = return $ SimplifiedConstBool False
simplifyExpr (Neg expr) = do
  e <- simplifyExpr expr
  createNameCall "neg" [e]
simplifyExpr (Not expr) = do
  e <- simplifyExpr expr
  createNameCall "not" [e]
simplifyExpr (EMul expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getMulOpName op) [e1, e2]
simplifyExpr (ERel expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getRelOpName op) [e1, e2]
simplifyExpr (EAdd expr1 op expr2) = do
  e1 <- simplifyExpr expr1
  e2 <- simplifyExpr expr2
  createNameCall (getAddOpName op) [e1, e2]
simplifyExpr (EApp (Ident name) exprs) = do
  exprsSimpl <- mapM simplifyExpr exprs
  createNameCall name exprsSimpl