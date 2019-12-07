{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.ASTUtils where

import           Syntax.Base

import           Control.Monad
import           Data.Foldable
import           Data.List

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.TypeExpressionResolver

checkType :: String -> SimplifiedExpr -> Infer SimplifiedExpr
checkType typeExpression expr = do
  declType <- parseTypeExpression typeExpression
  return $ SimplifiedCheck expr declType

declareTypes :: [(String, String)] -> SimplifiedExpr -> Infer SimplifiedExpr
declareTypes decls e = do
  foldrM (\(nameStr, typeStr) acc -> do
      declType <- parseTypeExpression typeStr
      return $ SimplifiedLet (Ident nameStr) (SimplifiedTyped declType) acc) e decls

valueOfType :: String -> Infer SimplifiedExpr
valueOfType typeExpression = do
  declType <- parseTypeExpression typeExpression
  return $ SimplifiedTyped declType

createCall :: SimplifiedExpr -> [SimplifiedExpr] -> Infer SimplifiedExpr
createCall fn args = do
  foldM (\acc arg -> return $ SimplifiedCall acc arg) fn args

createNameCall :: String -> [SimplifiedExpr] -> Infer SimplifiedExpr
createNameCall name args = createCall (SimplifiedVariable $ Ident name) args

getLambdaArgs :: [(String, String)] -> [String]
getLambdaArgs [] = ["Unit"]
getLambdaArgs args = map fst args

getLambdaArgsNames :: [(String, String)] -> [String]
getLambdaArgsNames [] = ["__nothing__"]
getLambdaArgsNames args = map snd args

createLambda :: [(String, String)] -> String -> SimplifiedExpr -> Infer SimplifiedExpr
createLambda args retType body = do
  lambda <- createUntypedLambda (getLambdaArgsNames args) body
  sig <- return $ intercalate " -> " $ getLambdaArgs args ++ [retType]
  checkType sig lambda

createUntypedLambda :: [String] -> SimplifiedExpr -> Infer SimplifiedExpr
createUntypedLambda argNames body = do
  idents <- return $ map (\name -> Ident name) argNames
  foldrM (\ident acc -> do
    return $ SimplifiedFunction ident acc) body idents