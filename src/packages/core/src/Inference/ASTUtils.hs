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

checkType :: (Traceable t) => String -> (SimplifiedExpr t) -> Infer t (SimplifiedExpr t)
checkType typeExpression expr = do
  declType <- parseTypeExpression typeExpression
  return $ SimplifiedCheck expr declType

declareTypes :: (Traceable t) => [(String, String)] -> (SimplifiedExpr t) -> Infer t (SimplifiedExpr t)
declareTypes decls e = do
  foldrM (\(nameStr, typeStr) acc -> do
      declType <- parseTypeExpression typeStr
      return $ SimplifiedLet (Ident nameStr) (SimplifiedTyped declType) acc) e decls

valueOfType :: (Traceable t) => String -> Infer t (SimplifiedExpr t)
valueOfType typeExpression = do
  declType <- parseTypeExpression typeExpression
  return $ SimplifiedTyped declType

createCall :: (Traceable t) => (SimplifiedExpr t) -> [(SimplifiedExpr t)] -> Infer t (SimplifiedExpr t)
createCall fn args = do
  foldM (\acc arg -> return $ SimplifiedCall acc arg) fn args

createNameCall :: (Traceable t) => String -> [(SimplifiedExpr t)] -> Infer t (SimplifiedExpr t)
createNameCall name args = createCall (SimplifiedVariable $ Ident name) args

getLambdaArgs :: [(String, String)] -> [String]
getLambdaArgs [] = ["Unit"]
getLambdaArgs args = map fst args

getLambdaArgsNames :: [(String, String)] -> [String]
getLambdaArgsNames [] = ["__nothing__"]
getLambdaArgsNames args = map snd args

createLambda :: (Traceable t) => [(String, String)] -> String -> (SimplifiedExpr t) -> Infer t (SimplifiedExpr t)
createLambda args retType body = do
  lambda <- createUntypedLambda (getLambdaArgsNames args) body
  sig <- return $ intercalate " -> " $ getLambdaArgs args ++ [retType]
  checkType sig lambda

createUntypedLambda :: (Traceable t) => [String] -> (SimplifiedExpr t) -> Infer t (SimplifiedExpr t)
createUntypedLambda argNames body = do
  idents <- return $ map (\name -> Ident name) argNames
  foldrM (\ident acc -> do
    return $ SimplifiedFunction ident acc) body idents