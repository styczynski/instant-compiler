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

declareTypes :: [(String, String)] -> SimplifiedExpr -> Infer SimplifiedExpr
declareTypes decls e = do
  foldM (\acc (nameStr, typeStr) -> do
      declType <- parseTypeExpression typeStr
      return $ SimplifiedLet (Ident nameStr) (SimplifiedTyped declType) acc) e decls

createCall :: SimplifiedExpr -> [SimplifiedExpr] -> Infer SimplifiedExpr
createCall fn args = do
  foldrM (\arg acc -> return $ SimplifiedCall acc arg) fn args

createNameCall :: String -> [SimplifiedExpr] -> Infer SimplifiedExpr
createNameCall name args = createCall (SimplifiedVariable $ Ident name) args

createLambda :: [(String, String)] -> String -> SimplifiedExpr -> Infer SimplifiedExpr
createLambda args retType body = do
  argNames <- return $ map snd args
  lambda <- createUntypedLambda argNames body
  sig <- return $ intercalate " -> " $ (map fst args) ++ [retType]
  return $ SimplifiedAnnotated sig lambda

createUntypedLambda :: [String] -> SimplifiedExpr -> Infer SimplifiedExpr
createUntypedLambda argNames body = do
  idents <- return $ map (\name -> Ident name) argNames
  foldrM (\ident acc -> do
    return $ SimplifiedFunction ident acc) body idents