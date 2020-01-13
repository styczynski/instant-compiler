{-|
Module      : Inference.inferencerUtils
Description : Helpful utilities to handle inference
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides basic utilities like fresh name generator, type normalization,
  and tracing helpers.
-}
{-# LANGAUGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.InferencerUtils where

import           Syntax.Base             hiding ( TV
                                                , TypeConstraint
                                                )
import qualified Syntax.Base                   as Syntax

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.ConstraintSolver

import           Data.List                      ( nub )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Injects current AST annotation into the simplified AST node
addExprAnnot :: (AST r t) => Infer r t (SimplifiedExpr r t) -> Infer r t (SimplifiedExpr r t)
addExprAnnot inExpr = do
  s             <- get
  inferTraceTop <- return $ inferTrace s
  e <- inExpr
  return $ SimplifiedAnnotated inferTraceTop e

typeRemember :: (AST r t) => Type -> Infer r t ()
typeRemember t = do
  meta <- return $ getTypeMeta t
  case meta of
    (TypeMetaNone) -> return ()
    (TypeMeta ids) -> do
      env <- get
      newTypeMap <- return $ foldl (\acc id -> Map.insertWith (\_ old -> old) id (schemeToStr $ Scheme [] t) acc) (typeMap env) ids
      _ <- liftIO $ putStrLn $ "Now: " ++ (show ids) ++ " -> " ++ (schemeToStr $ Scheme [] t)
      put $ env { typeMap = newTypeMap }
      return ()

-- | Adds new inference trace node (for debugging purposes)
markTrace :: (AST r t) => (SimplifiedExpr r t) -> t -> Infer r t ()
markTrace expr a = do
  s          <- get
  put s { inferTrace = (inferTrace s) ++ [ TypeErrorPayload a ] }
  return ()

-- | Removes inference trace node (for debugging purposes)
-- TODO: This is fucked up
unmarkTrace :: (AST r t, Show a, Print a) => (SimplifiedExpr r t) -> a -> Infer r t ()
unmarkTrace _ a = do
  s          <- get
  put s { inferTrace = (inferTrace s) ++ [ EmptyPayload ] }
  return ()
--  s        <- get
--  newTrace <-
--    return
--      $ let InferState { inferTrace = inferTrace } = s in drop 1 inferTrace
--  put s { inferTrace = newTrace }
--  return ()

-- | Generates error payload for current inference trace (for debugging purposes)
errPayload :: (AST r t) => Infer r t [TypeErrorPayload t]
errPayload = do
  s            <- get
  lastTraceStr <- return $ inferTrace s
  return $ lastTraceStr

-- | Performs env ?? name operation but throws error if it fails
lookupEnv :: (AST r t) => Ident -> Infer r t Type
lookupEnv name = do
  env <- ask
  case (env ?? name) of
    Nothing -> do
      payl <- errPayload
      throwError $ UnboundVariable payl name env
    (Just (Scheme vars typeScheme)) -> do
      varsMapped <- mapM (const freshTypeVar) vars
      return $ (Subst $ Map.fromList $ zip vars varsMapped) .> typeScheme

-- | Generate names for polymoprhic variable names
letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

-- | Generate unique indentificator for variable
freshIdent :: (AST r t) => Infer r t Ident
freshIdent = do
  s <- get
  put s { count = count s + 1 }
  return $ Ident $ "__@$internal_variable__" ++ (letters !! count s) ++ "_"

-- | Generate unique identificator for type variable
freshTypeVar :: (AST r t) => Infer r t Type
freshTypeVar = do
  s <- get
  put s { count = count s + 1 }
  return $ TypeVar TypeMetaNone $ TV (letters !! count s)

freshCountInt :: (AST r t) => Infer r t Int
freshCountInt = do
  s <- get
  put s { tCount = tCount s + 1 }
  return $ tCount s

getTagIndex :: (AST r t) => String -> Infer r t Int
getTagIndex tagName = do
  s <- get
  case Map.lookup tagName (tagMap s) of
    (Just i) -> return i
    Nothing  -> do
      c <- freshCountInt
      s <- get
      put s { tagMap = Map.insert tagName c (tagMap s) }
      return c

freshTypeVarPlaceholders :: (AST r t) => Int -> Infer r t [Type]
freshTypeVarPlaceholders n = do
  r <- foldrM
    (\_ acc -> do
      tv <- freshTypeVar
      return $ acc ++ [tv]
    )
    []
    (replicate n 0)
  return r

freshTypeVarPlaceholdersLock :: (AST r t) => Int -> Infer r t [Type]
freshTypeVarPlaceholdersLock n = do
  r <- foldrM
    (\_ acc -> do
      tv <- return $ TypeUnit TypeMetaNone
      return $ acc ++ [tv]
    )
    []
    (replicate n 0)
  return r

-- | Injects type containt statement into AST
constraintAnnot :: (AST r t) => TypeConstraint r t -> Infer r t (TypeConstraint r t)
constraintAnnot (TypeConstraint _ constrnt) = do
  payl <- errPayload
  return $ TypeConstraint payl constrnt

-- | Injects type constraint statement into AST for each node in the list
constraintAnnoTypeList :: (AST r t) => [TypeConstraint r t] -> Infer r t [TypeConstraint r t]
constraintAnnoTypeList cs = do
  foldrM
    (\c acc -> do
      ca <- constraintAnnot c
      return $ [ca] ++ acc
    )
    []
    cs

class Normalizable a where
  normalize :: a -> a

class NormalizableWith a b where
  normalizeWith :: [(a,a)] -> b -> b

-- | Helper to normalize type free variables
instance NormalizableWith TypeVar Type where
  normalizeWith _ (TypeUnit r)          = TypeUnit r
  normalizeWith _ (TypeAnnotated r v) = (TypeAnnotated r v)
  normalizeWith ord (TypeArrow r a b) =
    TypeArrow r (normalizeWith ord a) (normalizeWith ord b)
  normalizeWith ord (TypeTuple r a b) =
    TypeTuple r (normalizeWith ord a) (normalizeWith ord b)
  normalizeWith ord (TypeList r a) = TypeList r (normalizeWith ord a)
  normalizeWith _   (TypeStatic r a) = TypeStatic r a
  normalizeWith ord (TypeComplex r name deps) =
    TypeComplex r name $ map (normalizeWith ord) deps
  normalizeWith ord (TypePoly r alternatives) =
    TypePoly r $ map (normalizeWith ord) alternatives
  normalizeWith ord (TypeVar r a) = case Prelude.lookup a ord of
    Just x  -> TypeVar r x
    Nothing -> error "Type variable does not exist in type signature"
  normalizeWith _ v = v

-- | Normalize type free variables
instance Normalizable Scheme where
  normalize (Scheme _ body) =
    let freeVars = (zip (nub $ freeDimensionsList body) (map TV letters))
    in  Scheme (map snd freeVars) (normalizeWith freeVars body)

