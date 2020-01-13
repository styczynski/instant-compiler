{-|
Module      : Inference.ContraintSolver
Description : Solver for  inference constraints
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This is code for contraint solver that tries
  to unify the types matching all provided contraints.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Inference.ConstraintSolver where

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Substitutions
import           Inference.Errors
import           Inference.Types

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Monad presenting solver computation
type Solve r t = StateT (SolverState r t) (ExceptT (TypeError r t) IO)

-- | Solver state
data (AST r t) => SolverState r t = SolverState {
  annotTrace :: [TypeErrorPayload t]
}

-- | Empty solver state
emptySolverState :: (AST r t) => SolverState r t
emptySolverState = SolverState { annotTrace = [] }

-- | Gets type constraint and records its annotation inside solver state
--   This is for purely debug purposes
checkpointAnnotSolve :: (AST r t) => TypeConstraint r t -> Solve r t ()
checkpointAnnotSolve (TypeConstraint l _) = do
  s <- get
  put s { annotTrace = l }
  return ()

-- | Generate error payload from current solver state (for debug purposes)
errSolvePayload :: (AST r t) => Solve r t [TypeErrorPayload t]
errSolvePayload = do
  s         <- get
  lastAnnot <- return $ annotTrace s
  --lastAnnot <- return $ foldr (\el acc -> case el of
  --  EmptyPayload -> if (length acc) > 0 then init acc else []
  --  v -> acc ++ [v]) [] (annotTrace s)
  --lastAnnot <- return $ foldl (\x y -> y:x) [] lastAnnot
  return $ lastAnnot

-- | Runs solve monad for given contraints
runSolve :: (AST r t) => [TypeConstraint r t] -> IO (Either (TypeError r t) TypeSubstitution)
runSolve cs = do
  r <- runExceptT (runStateT (solver (TypeUnifier cs emptySubst)) emptySolverState)
  case r of
    Left  e      -> return $ Left e
    Right (s, _) -> return $ Right s

-- | Runs solver to unify all types
solver :: (AST r t) => (TypeUnifier r t) -> Solve r t TypeSubstitution
solver (TypeUnifier cs su) = case cs of
  [] -> return su
  ((TypeConstraint l (typeArgA, typeArgB)) : cs0) -> do
    -- TODO: Remove comments
    --_ <- liftIO $ liftIO $ liftIO $ putStrLn "AAAAAA"
    --_ <- liftIO $ liftIO $ liftIO $ putStrLn $ show l
    checkpointAnnotSolve (TypeConstraint l (typeArgA, typeArgB))
    su1 <- typeArgA <-$-> typeArgB
    solver $ TypeUnifier (su1 .> cs0) (su1 +> su)

-- | Represents a data type that can bind values of one type to the other one
class (AST r t) => BindableSolve r t a b where
  (<-$->) :: a -> b -> Solve r t TypeSubstitution

-- | This instance represents binding type variables to their types (creating contraints)
instance (AST r t) => BindableSolve r t TypeVar Type where
  (<-$->) a t = do
    payl <- errSolvePayload
    case a <-> t of
      Left (InfiniteType _ a t) -> throwError $ InfiniteType payl a t
      (Left  v)                 -> throwError v
      (Right r)                 -> return r

-- | This instance represents binding type to type (unification)
instance (AST r t) => BindableSolve r t Type Type where
  (<-$->) typeArgA typeArgB | (clearMeta typeArgA) == (clearMeta typeArgB)                    = return emptySubst
  (<-$->) (TypeVar _ v)       t                 = v <-$-> t
  (<-$->) t                 (TypeVar  _ v     ) = v <-$-> t
  (<-$->) (TypeList _ typeArgA    ) (TypeList _ typeArgB    ) = [typeArgA] <-$-> [typeArgB]
  (<-$->) (TypeTuple _ typeArgA typeArgB) (TypeTuple _ typeArgC typeArgD) = [typeArgA, typeArgB] <-$-> [typeArgC, typeArgD]
  (<-$->) (TypeArrow _ typeArgA typeArgB) (TypeArrow _ typeArgC typeArgD) = [typeArgA, typeArgB] <-$-> [typeArgC, typeArgD]
  (<-$->) typeArgA@(TypePoly _ alternatives1) typeArgB@(TypePoly _ alternatives2) =
    alternatives1 <-$-> alternatives2
  (<-$->) typeArgA@(TypeComplex _ name1 deps1) typeArgB@(TypeComplex _ name2 deps2) = do
    payl <- errSolvePayload
    _    <- if not (name1 == name2)
      then throwError $ UnificationMismatch payl [typeArgA] [typeArgB]
      else return 0
    deps1 <-$-> deps2
  (<-$->) typeArgA typeArgB = do
    payl <- errSolvePayload
    throwError $ UnificationFail payl typeArgA typeArgB

-- | It's useful to bind multiple types at the same time
instance (AST r t) => BindableSolve r t [Type] [Type] where
  (<-$->) []        []        = return emptySubst
  (<-$->) (h1 : typeArgA) (h2 : typeArgB) = do
    uni1 <- h1 <-$-> h2
    uni2 <- (uni1 .> typeArgA) <-$-> (uni1 .> typeArgB)
    return $ uni2 +> uni1
  (<-$->) typeArgA typeArgB = do
    payl <- errSolvePayload
    throwError $ UnificationMismatch payl typeArgA typeArgB

