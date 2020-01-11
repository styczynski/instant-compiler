{-|
Module      : Inference.TypeExpressionResolver
Description : Resolver for type expressions
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module implements utilities to map AST type expressions into Inference.Types values.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.TypeExpressionResolver where


import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.ConstraintSolver
import           Inference.InferencerUtils

import           Syntax.Base

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           System.IO.Unsafe

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set


-- | Extracts free variables from type expression and assigns them to fresh
--   type variables.
getTypeSimpleExpressionFV
  :: (AST r t) => TypeSimpleExpression ASTMetadata -> Infer r t (Map.Map String TypeVar)
getTypeSimpleExpressionFV (TypeSExprList _ listType) =
  getTypeExpressionFV listType
getTypeSimpleExpressionFV (TypeSExprIdent _ _) = return Map.empty
getTypeSimpleExpressionFV (TypeSExprEmpty _) = return Map.empty
getTypeSimpleExpressionFV (TypeSExprAbstract _ (TypeIdentAbstract name)) = do
  tvv <- freshTypeVar
  return $ let (TypeVar _ tv) = tvv in Map.singleton name tv

getTypeExpressionFV :: (AST r t) => TypeExpression ASTMetadata -> Infer r t (Map.Map String TypeVar)
getTypeExpressionFV (TypeExprSimple _ simpl) = getTypeSimpleExpressionFV simpl
getTypeExpressionFV (TypeExprIdent _ (TypeArgJustOne _ param) _) =
  getTypeSimpleExpressionFV param
getTypeExpressionFV (TypeExprIdent _ (TypeArgJust _ firstParam restParams) _) = do
  foldlM
    (\acc (TypeArgEl _ el) -> do
      r <- getTypeExpressionFV el
      return $ acc `Map.union` r
    )
    Map.empty
    ([firstParam] ++ restParams)
getTypeExpressionFV (TypeFun _ a b) = do
  t1 <- (getTypeExpressionFV a)
  t2 <- (getTypeExpressionFV b)
  return $ t1 `Map.union` t2
getTypeExpressionFV (TypeExprTuple _ fstEl restEls) = foldlM
  (\acc el -> do
    t <- (getTypeExpressionFV el)
    return $ acc `Map.union` t
  )
  Map.empty
  ([fstEl] ++ restEls)
getTypeExpressionFV _ = return Map.empty

-- | Extracts free variables from type expression
instance (AST r t) => WithFreedomM (TypeSimpleExpression ASTMetadata) String (Infer r t) where
  freeDimensionsM (TypeSExprEmpty _)      = return $ Set.empty
  freeDimensionsM (TypeSExprList _ expr) = freeDimensionsM expr
  freeDimensionsM (TypeSExprAbstract _ (TypeIdentAbstract name)) =
    return $ Set.singleton name
  freeDimensionsM (TypeSExprIdent _ _) = return $ Set.empty

instance (AST r t) => WithFreedomM (TypeExpression ASTMetadata) String (Infer r t) where
  freeDimensionsM (TypeExprSimple _ simpl) = freeDimensionsM simpl
  freeDimensionsM (TypeExprIdent _ (TypeArgJustOne _ simpl) _) =
    freeDimensionsM simpl
  freeDimensionsM (TypeExprIdent _ (TypeArgJust _ firstParam restParams) _) = do
    foldlM
      (\acc (TypeArgEl _ el) -> do
        r <- freeDimensionsM el
        return $ acc `Set.union` r
      )
      Set.empty
      ([firstParam] ++ restParams)
  freeDimensionsM (TypeFun _ a b) = do
    x <- freeDimensionsM a
    y <- freeDimensionsM b
    return $ x `Set.union` y
  freeDimensionsM (TypeExprTuple _ firstElem restElems) = do
    x <- freeDimensionsM firstElem
    foldrM
      (\el acc -> do
        y <- freeDimensionsM el
        return $ acc `Set.union` y
      )
      x
      restElems

resolveTypeSimpleExpressionRec
  :: (AST r t) => (Map.Map String TypeVar) -> TypeSimpleExpression ASTMetadata -> Infer r t Type
resolveTypeSimpleExpressionRec fvs (TypeSExprEmpty _) = return $ TypeUnit TypeMetaNone
resolveTypeSimpleExpressionRec fvs (TypeSExprIdent _ (Ident name)) =
  return $ TypeStatic TypeMetaNone name
resolveTypeSimpleExpressionRec fvs (TypeSExprList _ expr) = do
  t <- resolveTypeExpressionRec fvs expr
  return $ TypeList TypeMetaNone t
resolveTypeSimpleExpressionRec fvs (TypeSExprAbstract _ (TypeIdentAbstract name))
  = do
    parsedName <- return $ [ x | x <- name, not (x `elem` "'") ]
    validFvs   <- return $ name `Map.member` fvs
    if not validFvs
      then do
        payl <- errPayload
        throwError
          $  Debug payl
          $  "Type name "
          ++ name
          ++ " is not a valid polymorhic type name"
      else let (Just tv) = Map.lookup name fvs in return $ TypeVar TypeMetaNone tv

resolveTypeExpressionRec
  :: (AST r t) => (Map.Map String TypeVar) -> TypeExpression ASTMetadata -> Infer r t Type
resolveTypeExpressionRec fvs (TypeExprSimple _ simpl) =
  resolveTypeSimpleExpressionRec fvs simpl
resolveTypeExpressionRec fvs (TypeExprIdent _ (TypeArgJust _ firstParam restParams) (Ident name))
  = do
    typeParams <- foldlM
      (\acc (TypeArgEl _ expr) -> do
        t <- resolveTypeExpressionRec fvs expr
        return $ [t] ++ acc
      )
      ([])
      ([firstParam] ++ restParams)
    return $ TypeComplex TypeMetaNone name typeParams
resolveTypeExpressionRec fvs (TypeExprIdent _ (TypeArgJustOne _ param) (Ident name))
  = do
    typeParam <- resolveTypeSimpleExpressionRec fvs param
    return $ TypeComplex TypeMetaNone name [typeParam]
resolveTypeExpressionRec fvs (TypeFun _ a b) = do
  t1 <- resolveTypeExpressionRec fvs a
  t2 <- resolveTypeExpressionRec fvs b
  return $ TypeArrow TypeMetaNone t1 t2
resolveTypeExpressionRec fvs (TypeExprTuple _ fstEl restEls) = do
  tupleT <- foldlM
    (\acc expr -> do
      t <- resolveTypeExpressionRec fvs expr
      return $ TypeTuple TypeMetaNone t acc
    )
    (TypeTuple TypeMetaNone (TypeUnit TypeMetaNone) (TypeUnit TypeMetaNone))
    ([fstEl] ++ restEls)
  return tupleT

-- | Entrypoint to resolve type expression
resolveTypeExpression :: (AST r t) => TypeExpression ASTMetadata -> Infer r t Scheme
resolveTypeExpression exp = do
  fvs  <- getTypeExpressionFV exp
  t    <- resolveTypeExpressionRec fvs exp
  fvsT <- return $ Map.elems fvs
  return $ Scheme fvsT t

-- | Parse type string with environment
parseTypeExpression
  :: (AST r t) =>  String
  -> Infer r t Scheme
parseTypeExpression typeExpr =
  let ts = myLexer typeExpr
  in
    case pTypeExpression ts of
      Ok tree -> resolveTypeExpression $ fmap (\_ -> EmptyMetadata) tree
      Bad t -> do
        _ <- liftIO $ liftIO $ liftIO $ putStrLn $ typeExpr
        _ <- liftIO $ liftIO $ liftIO $ putStrLn $ show t
        return $ Scheme [] (TypeUnit TypeMetaNone)
