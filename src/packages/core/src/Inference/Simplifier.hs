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

instance AST (Program ASTMetadata) (ASTNode ASTMetadata) where
  getEmptyPayload _ = ASTNone EmptyMetadata
  simplify p = simplifyProgram p
  describeErrors r payl = intercalate "" $ map (\p -> describeError r p) payl
  describeTraceItem r (ASTProgram _ ast) = "Program entry"
  describeTraceItem r (ASTTopDef _ ast@(FnDef meta typ (Ident name) arg _)) = "Top definition for \"" ++ name ++ "\": " ++ (prettifyErrorMessage True $ printTree $ FnDef meta typ (Ident name) arg (Block EmptyMetadata []))
  describeTraceItem r (ASTTypeName _ ast) = "Type name: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTStmt _ ast) = "Block statement: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTBlock _ ast) = "Block of code"
  describeTraceItem r (ASTItem _ ast) = "Item definition: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r (ASTExpr _ ast) = "Expression: " ++ (prettifyErrorMessage False $ printTree ast)
  describeTraceItem r _ = "Unknown AST part"

describeError :: Program ASTMetadata -> ASTNode ASTMetadata -> String
describeError r (ASTProgram _ ast) = printTree ast
describeError r (ASTExpr _ ast) = printTree ast
describeError r (ASTStmt _ p) = printTree p
describeError r (ASTBlock _ p) = printTree p
describeError r (ASTTopDef _ p) = printTree p
describeError r (ASTTypeName _ p) = printTree p
describeError r (ASTItem _ p) = printTree p
describeError r (ASTNone _) = "<none>"


getPreface :: (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
getPreface p = do
  declareTypes [
    ("ignore", "'a -> Void"),
    ("printInt", "Int -> Void"),
    ("then", "'a -> 'b -> 'b"),
    ("not", "Bool -> Bool"),
    ("neg", "Int -> Int"),
    ("+", "Int -> Int -> Int"),
    ("++", "Int -> Int"),
    ("-", "Int -> Int -> Int"),
    ("/", "Int -> Int -> Int"),
    ("*", "Int -> Int -> Int"),
    ("%", "Int -> Int -> Int"),
    ("=", "'a -> 'a -> 'a"),
    ("<", "'a -> 'a -> Bool"),
    (">", "'a -> 'a -> Bool"),
    ("<=", "'a -> 'a -> Bool"),
    (">=", "'a -> 'a -> Bool"),
    ("==", "'a -> 'a -> Bool"),
    ("!=", "'a -> 'a -> Bool")] p

simplifyProgram :: (Program ASTMetadata) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyProgram (Program _ defs) = do
  u <- valueOfType "Void"
  p0 <- foldrM (\def acc -> simplifyTopDef def acc) (SimplifiedCall (SimplifiedVariable $ Ident "main") u) defs
  p <- getPreface p0
  --_ <- liftIO $ putStrLn $ show p
  return p

simplifyTopDef :: TopDef ASTMetadata -> (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyTopDef ast@(FnDef meta retType name args body) expr = do
  markTrace expr $ ASTTopDef meta ast
  b <- simplifyBlock body expr
  l <- createLambda (map (\(Arg EmptyMetadata typeName (Ident argName)) -> (getTypeName typeName, argName)) args) (getTypeName retType) b
  tl <- return $ SimplifiedLet name l expr
  --_ <- liftIO $ putStrLn $ "\n\nTop def:\n"
  --_ <- liftIO $ putStrLn $ show tl
  r <- addExprAnnot $ return tl
  unmarkTrace expr ast
  return r

simplifyBlock :: Block ASTMetadata -> (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyBlock ast@(Block meta statements) expr = do
  markTrace expr $ ASTBlock meta ast
  u <- valueOfType "Void"
  r <- foldrM (\stmt acc -> simplifyStatement acc stmt) u statements
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r

simplifyStatement :: (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Stmt ASTMetadata -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyStatement expr ast@(Decl meta typeName inits) = do
  markTrace expr $ ASTStmt meta ast
  r <- foldM (simplifyStatementDecl typeName) expr inits
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(Ass meta name exprVal) = do
  markTrace expr $ ASTStmt meta ast
  valSimpl <- simplifyExpr exprVal SimplifiedSkip
  callSimpl <-  return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "=")) (SimplifiedVariable name)) valSimpl
  r <- return $ SimplifiedLet name callSimpl expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(While meta exprCond stmtExpr) = do
  markTrace expr $ ASTStmt meta ast
  condSimpl <- simplifyExpr exprCond SimplifiedSkip
  bodySimpl <- simplifyStatement SimplifiedSkip stmtExpr
  whileCheckType <- return $ Scheme [] $ (TypeStatic "Bool")
  condSimplChecked <- return $ SimplifiedCheck condSimpl whileCheckType
  id <- freshIdent
  r <- return $ SimplifiedLet id condSimplChecked bodySimpl
  r <- return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) r) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(CondElse meta exprCond stmtThen stmtElse) = do
  markTrace expr $ ASTStmt meta ast
  condSimpl <- simplifyExpr exprCond SimplifiedSkip
  thenSimpl <- simplifyStatement SimplifiedSkip stmtThen
  elseSimpl <- simplifyStatement SimplifiedSkip stmtElse
  r <- return $ SimplifiedIf condSimpl (SimplifiedCall (SimplifiedVariable $ Ident "ignore") thenSimpl) (SimplifiedCall (SimplifiedVariable $ Ident "ignore") elseSimpl)
  r <- return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) r) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(Cond meta exprCond stmtThen) = do
  markTrace expr $ ASTStmt meta ast
  condSimpl <- simplifyExpr exprCond SimplifiedSkip
  thenSimpl <- simplifyStatement SimplifiedSkip stmtThen
  elseSimpl <- valueOfType "Void"
  r <- return $ SimplifiedIf condSimpl (SimplifiedCall (SimplifiedVariable $ Ident "ignore") thenSimpl) (SimplifiedCall (SimplifiedVariable $ Ident "ignore") elseSimpl)
  r <- return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) r) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(SExp meta stmtExpr) = do
  markTrace expr $ ASTStmt meta ast
  e <- simplifyExpr stmtExpr expr
  r <- return $ SimplifiedCall ((SimplifiedCall (SimplifiedVariable $ Ident "then")) e) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(BStmt meta block) = do
   markTrace expr $ ASTStmt meta ast
   r <- simplifyBlock block expr
   r <- addExprAnnot $ return r
   unmarkTrace expr ast
   return r
simplifyStatement expr ast@(Incr meta ident) = do
  markTrace expr $ ASTStmt meta ast
  rVal <- return $ SimplifiedCall (SimplifiedVariable $ Ident "++") (SimplifiedVariable ident)
  r <- return $ SimplifiedLet ident rVal expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(VRet meta) = do
  markTrace expr $ ASTStmt meta ast
  r <- valueOfType "Void"
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatement expr ast@(Ret meta retExpr) = do
  markTrace expr $ ASTStmt meta ast
  r <- simplifyExpr retExpr expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r

getTypeDefaultValue :: (AST r t) => TypeName ASTMetadata -> (SimplifiedExpr r t)
getTypeDefaultValue (Int _) = SimplifiedConstInt 0
getTypeDefaultValue (Str _) = SimplifiedConstString ""
getTypeDefaultValue (Bool _) = SimplifiedConstBool False

getTypeName :: TypeName ASTMetadata -> String
getTypeName (Int _) = "Int"
getTypeName (Bool _) = "Bool"
getTypeName (Str _) = "String"
getTypeName (Void _) = "Void"

getTypeDefaultAnnot :: (AST r t) => TypeName ASTMetadata -> (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
getTypeDefaultAnnot typeName e = checkType (getTypeName typeName) e

simplifyStatementDecl :: TypeName ASTMetadata -> (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Item ASTMetadata -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyStatementDecl typeName expr ast@(NoInit meta name) = do
  markTrace expr $ ASTItem meta ast
  r <- return $ SimplifiedLet name (getTypeDefaultValue typeName) expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r
simplifyStatementDecl typeName expr ast@(Init meta name initExpr) = do
  markTrace expr $ ASTItem meta ast
  e <- simplifyExpr initExpr expr
  annotExp <- getTypeDefaultAnnot typeName e
  r <- return $ SimplifiedLet name annotExp expr
  r <- addExprAnnot $ return r
  unmarkTrace expr ast
  return r


getMulOpName :: MulOp ASTMetadata -> String
getMulOpName (Times _) = "*"
getMulOpName (Div _) = "/"
getMulOpName (Mod _) = "%"

getAddOpName :: AddOp ASTMetadata -> String
getAddOpName (Plus _) = "+"
getAddOpName (Minus _) = "-"

getRelOpName :: RelOp ASTMetadata -> String
getRelOpName (LTH _) = "<"
getRelOpName (LE _) = "<="
getRelOpName (GTH _) = ">"
getRelOpName (GE _) = ">="
getRelOpName (LTH _) = "<"
getRelOpName (GTH _) = ">"
getRelOpName (EQU _) = "=="
getRelOpName (NE _) = "!="

getAppArgs :: (AST r t) => [(SimplifiedExpr r t)] -> Infer r t [(SimplifiedExpr r t)]
getAppArgs [] = do
  u <- valueOfType "Void"
  return [u]
getAppArgs args = return args

simplifyExpr :: Expr ASTMetadata -> (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyExpr e expr = do
  markTrace expr $ ASTExpr EmptyMetadata e
  r <- simplifyExpr_ e expr
  r <- addExprAnnot $ return r
  unmarkTrace expr e
  return r


simplifyExpr_ :: Expr ASTMetadata -> (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata)) -> Infer (Program ASTMetadata) (ASTNode ASTMetadata) (SimplifiedExpr (Program ASTMetadata) (ASTNode ASTMetadata))
simplifyExpr_ (EVar _ name) _ = return $ SimplifiedVariable name
simplifyExpr_ (ELitInt _ val) _ = return $ SimplifiedConstInt val
simplifyExpr_ (EString _ val) _ = return $ SimplifiedConstString val
simplifyExpr_ (ELitTrue _) _ = return $ SimplifiedConstBool True
simplifyExpr_ (ELitFalse _) _ = return $ SimplifiedConstBool False
simplifyExpr_ (Neg _ expr) e = do
  e <- simplifyExpr expr e
  createNameCall "neg" [e]
simplifyExpr_ (Not _ expr) e = do
  e <- simplifyExpr expr e
  createNameCall "not" [e]
simplifyExpr_ (EMul _ expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getMulOpName op) [e1, e2]
simplifyExpr_ (ERel _ expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getRelOpName op) [e1, e2]
simplifyExpr_ (EAdd _ expr1 op expr2) e = do
  e1 <- simplifyExpr expr1 e
  e2 <- simplifyExpr expr2 e
  createNameCall (getAddOpName op) [e1, e2]
simplifyExpr_ (EApp _ (Ident name) exprs) e = do
  exprsSimpl <- mapM (\expr -> simplifyExpr expr e) exprs
  args <- getAppArgs exprsSimpl
  createNameCall name args