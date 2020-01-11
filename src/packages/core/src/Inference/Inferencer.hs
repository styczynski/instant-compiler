{-|
Module      : Inference.Inferencer
Description : Main code for type inference
Copyright   : (c) Piotr Styczyński, 2019
License     : MIT
Maintainer  : piotr@styczynski.in
Stability   : experimental
Portability : POSIX

  This module provides basic code for translating simplified AST generated by
  Inference.Simplifier module, generating constraints on types and feeding them
  into Inference.ContraintSolver.
-}
module Inference.Inferencer where

import           Inference.Syntax
import           Inference.TypingEnvironment
import           Inference.Types
import           Inference.Substitutions
import           Inference.Errors
import           Inference.Simplifier
import           Inference.ConstraintSolver
import           Inference.InferencerUtils
import           Inference.TypeExpressionResolver

import           Syntax.Base             hiding ( TypeConstraint )
import qualified Syntax.Base                   as Syntax

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Identity
import           Data.Foldable

import           System.IO.Unsafe

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- | Runs inference monad
runInfer
  :: (AST r t) => TypeEnvironment
  -> InferState r t
  -> Infer r t (TypeEnvironment, Type, [TypeConstraint r t])
  -> IO
       ( Either
           (TypeError r t)
           ((TypeEnvironment, Type, [TypeConstraint r t]), InferState r t)
       )
runInfer env state fn = do
  v <- runExceptT (runReaderT (runStateT fn (state)) (env))
  case v of
    (Left  e                   ) -> return $ Left e
    (Right ((env, t, c), state)) -> return $ Right ((env, t, c), state)

-- | Runs solver monad
solve
  :: (AST r t) => Either (TypeError r t) (Type, [TypeConstraint r t]) -> IO (Either (TypeError r t) Scheme)
solve r = case r of
  Left  err      -> return $ Left err
  Right (ty, cs) -> do
    s <- runSolve cs
    case s of
      Left err -> return $ Left err
      Right subst ->
        return
          $  Right
          $  (normalize . (\l -> Scheme (generalized Inference.TypingEnvironment.empty l) l))
          $  subst
          .> ty

-- | Helper to remove state and env from inference monad output
unpackEnvTypeContraints
  :: (AST r t) => Either (TypeError r t) ((TypeEnvironment, Type, [TypeConstraint r t]), InferState r t)
  -> Either (TypeError r t) (Type, [TypeConstraint r t])
unpackEnvTypeContraints (Left  r             ) = Left r
unpackEnvTypeContraints (Right ((_, t, c), _)) = Right (t, c)

-- | Helper to extract environment from inference monad output
retrieveEnv
  :: (AST r t) => Either (TypeError r t) ((TypeEnvironment, Type, [TypeConstraint r t]), InferState r t)
  -> TypeEnvironment
retrieveEnv (Left  r             ) = empty
retrieveEnv (Right ((e, _, _), _)) = e

-- | Helper to extract inferencer state from inference monad output
retrieveState
  :: (AST r t) => Either (TypeError r t) ((TypeEnvironment, Type, [TypeConstraint r t]), InferState r t)
  -> InferState r t
--retrieveState (Left  r         ) = initInfer
retrieveState (Right (_, state)) = state

-- | Takes AST and run inferencer and solver returning inferenced type
inferAST
  :: (AST r t) => TypeEnvironment
  -> InferState r t
  -> r
  -> IO (Either (TypeError r t) (Scheme, TypeEnvironment, InferState r t))
inferAST env state ex = do
  i      <- runInfer env state (inferProgram ex)
  env    <- return $ retrieveEnv i
  state  <- return $ retrieveState i
  scheme <- solve $ unpackEnvTypeContraints i
  case scheme of
    Left  e -> return $ Left e
    Right s -> return $ Right (s, env, state)

-- | Get type contraints for given implementation node in AST
inferProgram
  :: (AST r t) => r -> Infer r t (TypeEnvironment, Type, [TypeConstraint r t])
inferProgram ast = do
  simpl <- simplify ast
  -- _ <- liftIO $ liftIO $ liftIO $ putStrLn $ show simpl
  env <- ask
  (simplT, simplC) <- infer simpl
  return (env, simplT, simplC)

-- | Creates abstract type constructor for given name and parameters
createTypeExpressionAbstractArgConstructor
  :: Ident -> [String] -> TypeExpression ASTMetadata
createTypeExpressionAbstractArgConstructor typeName [] =
  TypeExprSimple EmptyMetadata $ TypeSExprIdent EmptyMetadata typeName
createTypeExpressionAbstractArgConstructor typeName names@(hNames : tNames) =
  let
    (identHead : identTail) = map
      (\e ->
        TypeArgEl EmptyMetadata $ TypeExprSimple EmptyMetadata $ TypeSExprAbstract EmptyMetadata $ TypeIdentAbstract e
      )
      names
  in  TypeExprIdent EmptyMetadata (TypeArgJust EmptyMetadata identHead identTail) typeName

-- | Transforms AST type param to list of parameters' names
typeParamsToList :: TypeParam ASTMetadata -> [String]
typeParamsToList (TypeParamNone _) = []
typeParamsToList (TypeParamJust _ names) =
  map (\(TypeIdentAbstract name) -> name) names
typeParamsToList (TypeParamJustOne _ (TypeIdentAbstract name)) = [name]

------------------------------------------------------------------
--        Inference for various types of AST nodes              --
------------------------------------------------------------------

inferE :: (AST r t) => SimplifiedExpr r t -> Infer r t (TypeEnvironment, Type, [TypeConstraint r t])
inferE expr = do
  env    <- ask
  (t, c) <- infer expr
  return $ (env, t, c)

-- | Create constraint for two types (these two types will be unified by solver)
(<.>) :: (AST r t) => Type -> Type -> Infer r t (TypeConstraint r t)
(<.>) type1 type2 = do
  p <- errPayload
  return $ TypeConstraint p (type1, type2)

infer :: (AST r t) => SimplifiedExpr r t -> Infer r t (Type, [TypeConstraint r t])
infer SimplifiedSkip            = return ((TypeStatic TypeMetaNone "Void"), [])
infer (SimplifiedConstInt    _) = return ((TypeStatic TypeMetaNone "Int"), [])
infer (SimplifiedConstBool   _) = return ((TypeStatic TypeMetaNone "Bool"), [])
infer (SimplifiedConstString _) = return ((TypeStatic TypeMetaNone "String"), [])
infer (SimplifiedAnnotated l t) = do
  s <- get
  put s { inferTrace = l }
  infer t
infer (SimplifiedTyped (Scheme _ t)) = return (t, [])
infer (SimplifiedExportEnv         ) = do
  env <- ask
  return (TypeAnnotated TypeMetaNone (AnnotationEnv env), [])
infer (SimplifiedCheck e (Scheme _ t)) = do
  (type1, constraintype1) <- infer e
  bindExpr1     <- type1 <.> t
  ac       <- constraintAnnoTypeList [bindExpr1]
  return (type1, constraintype1 ++ ac)
infer (SimplifiedVariable x) = do
  t <- lookupEnv x
  return (t, [])
infer (SimplifiedFunction x e) = do
  typeVar     <- freshTypeVar
  (t, c) <- (x, Scheme [] typeVar) ==> (infer e)
  return (TypeArrow TypeMetaNone typeVar t, c)
infer (SimplifiedCall e1 e2) = do
  (type1, constraintype1) <- infer e1
  (type2, constraintype2) <- infer e2
  typeVar       <- freshTypeVar
  bindExpr1     <- type1 <.> (TypeArrow TypeMetaNone type2 typeVar)
  ac       <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ constraintype2 ++ ac)
infer (SimplifiedLetAs x meta e1 _ e2) = do
  (gt, gc) <- infer (SimplifiedLet x meta e1 e2)
  env      <- ask
  typeVar       <- freshTypeVar
  (type1, constraintype1) <- infer e1
  bindExpr1     <- gt <.> type1
  ac       <- constraintAnnoTypeList [bindExpr1]
  s        <- lift $ lift $ lift $ runSolve constraintype1
  case s of
    Left  err -> throwError err
    Right sub -> do
      let sc = Scheme (generalized (sub .> env) (sub .> type1)) (sub .> type1)
      (type2, constraintype2) <-
        (x, sc) ==> (local (sub .>) (infer (SimplifiedTyped $ Scheme [] typeVar)))
      return (type2, ac ++ constraintype1 ++ constraintype2)
infer (SimplifiedLet x meta e1 e2) = do
  env      <- ask
  (type1, constraintype1) <- infer e1
  s        <- lift $ lift $ lift $ runSolve constraintype1
  case s of
    Left  err -> throwError err
    Right sub -> do
      let sc = Scheme (generalized (sub .> env) (sub .> type1)) (sub .> type1)
      (type2, constraintype2) <- (x, sc) ==> (local (sub .>) (infer e2))
      _ <- liftIO $ putStrLn $ "Infer let: [" ++ (show meta) ++ "] ."
      return (type2, constraintype1 ++ constraintype2)
infer (SimplifiedFixPoint e1) = do
  (type1, constraintype1) <- infer e1
  typeVar       <- freshTypeVar
  bindExpr1     <- (TypeArrow TypeMetaNone typeVar typeVar) <.> type1
  ac       <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ ac)
infer (SimplifiedUnaryOp (OpCustomUni name) e1) = do
  infer (SimplifiedCall (SimplifiedVariable $ Ident name) e1)
infer (SimplifiedUnaryOp op e1) = do
  (type1, constraintype1) <- infer e1
  typeVar       <- freshTypeVar
  absType1       <- return $ TypeArrow TypeMetaNone type1 typeVar
  absType2       <- inferUnaryOperation op
  bindExpr1     <- absType1 <.> absType2
  ac       <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ ac)
infer (SimplifiedBinaryOp (OpCustom name) e1 e2) = do
  infer
    (SimplifiedCall (SimplifiedCall (SimplifiedVariable $ Ident name) e1) e2)
infer (SimplifiedBinaryOp op e1 e2) = do
  (type1, constraintype1) <- infer e1
  (type2, constraintype2) <- infer e2
  typeVar       <- freshTypeVar
  absType1       <- return $ TypeArrow TypeMetaNone type1 (TypeArrow TypeMetaNone type2 typeVar)
  absType2       <- inferBinaryOperation op
  bindExpr1     <- absType1 <.> absType2
  ac       <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ constraintype2 ++ ac)
infer (SimplifiedIf cond tr fl) = do
  (type1, constraintype1) <- infer cond
  (type2, constraintype2) <- infer tr
  (type3, constraint3) <- infer fl
  bindExpr1     <- type1 <.> (TypeStatic TypeMetaNone "Bool")
  bindExpr2     <- (type2 <.> type3)
  ac       <- constraintAnnoTypeList [bindExpr1, bindExpr2]
  return (type2, constraintype1 ++ constraintype2 ++ constraint3 ++ ac)
infer (SimplifiedAlternatives exps) = do
  typeVar <- freshTypeVar
  foldrM
    (\exp (tAcc, cAcc) -> do
      (tExp, cExp) <- infer exp
      bindExpr1         <- typeVar <.> tExp
      ac           <- constraintAnnoTypeList [bindExpr1]
      return (tExp, cAcc ++ cExp ++ ac)
    )
    (typeVar, [])
    exps
infer (SimplifiedTagUnpack (Ident name) exp) = do
  (type1, constraintype1) <- infer exp
  typeVar       <- freshTypeVar
  absType1       <- return $ TypeArrow TypeMetaNone type1 typeVar
  polyTypeVar       <- freshTypeVar
  polyC    <- getTagIndex name
  polyV1   <- freshTypeVarPlaceholdersLock (polyC + 1)
  polyV2   <- freshTypeVarPlaceholdersLock (50 - polyC)
  absType2       <-
    return
    $ TypeArrow TypeMetaNone (TypePoly TypeMetaNone $ polyV1 ++ [TypeComplex TypeMetaNone name [polyTypeVar]] ++ polyV2) polyTypeVar
  bindExpr1 <- absType1 <.> absType2
  ac   <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ ac)
infer (SimplifiedTagUnpackNonStrict (Ident name) exp) = do
  (type1, constraintype1) <- infer exp
  typeVar       <- freshTypeVar
  absType1       <- return $ TypeArrow TypeMetaNone type1 typeVar
  polyTypeVar       <- freshTypeVar
  polyC    <- getTagIndex name
  polyV1   <- freshTypeVarPlaceholders (polyC + 1)
  polyV2   <- freshTypeVarPlaceholders (50 - polyC)
  absType2       <-
    return
    $ TypeArrow TypeMetaNone (TypePoly TypeMetaNone $ polyV1 ++ [TypeComplex TypeMetaNone name [polyTypeVar]] ++ polyV2) polyTypeVar
  bindExpr1 <- absType1 <.> absType2
  ac   <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ ac)
infer (SimplifiedTag (Ident name) SimplifiedSkip) = do
  absType1     <- freshTypeVar
  polyTypeVar     <- return $ TypeUnit TypeMetaNone
  polyC  <- getTagIndex name
  polyV1 <- freshTypeVarPlaceholders (polyC + 1)
  polyV2 <- freshTypeVarPlaceholders (50 - polyC)
  absType2     <- return $ (TypePoly TypeMetaNone $ polyV1 ++ [TypeComplex TypeMetaNone name [polyTypeVar]] ++ polyV2)
  bindExpr1   <- absType1 <.> absType2
  ac     <- constraintAnnoTypeList [bindExpr1]
  return (absType1, ac)
infer (SimplifiedTag (Ident name) exp) = do
  (type1, constraintype1) <- infer exp
  typeVar       <- freshTypeVar
  absType1       <- return $ TypeArrow TypeMetaNone type1 typeVar
  polyTypeVar       <- freshTypeVar
  polyC    <- getTagIndex name
  polyV1   <- freshTypeVarPlaceholders (polyC + 1)
  polyV2   <- freshTypeVarPlaceholders (50 - polyC)
  absType2       <-
    return
    $ TypeArrow TypeMetaNone polyTypeVar (TypePoly TypeMetaNone $ polyV1 ++ [TypeComplex TypeMetaNone name [polyTypeVar]] ++ polyV2)
  bindExpr1 <- absType1 <.> absType2
  ac   <- constraintAnnoTypeList [bindExpr1]
  return (typeVar, constraintype1 ++ ac)

inferBinaryOperation :: (AST r t) => BinaryOp -> Infer r t Type
inferBinaryOperation OpSemicolon = do
  typeVar1 <- freshTypeVar
  typeVar2 <- freshTypeVar
  return $ TypeArrow TypeMetaNone typeVar1 (TypeArrow TypeMetaNone typeVar2 typeVar2)
inferBinaryOperation OpSame = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone typeVar (TypeArrow TypeMetaNone typeVar typeVar)
inferBinaryOperation OpCons = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone (typeVar) (TypeArrow TypeMetaNone (TypeList TypeMetaNone typeVar) (TypeList TypeMetaNone typeVar))
inferBinaryOperation OpTupleCons = do
  typeVar  <- freshTypeVar
  typeVar2 <- freshTypeVar
  typeVar3 <- freshTypeVar
  return
    $ TypeArrow TypeMetaNone (typeVar) (TypeArrow TypeMetaNone (TypeTuple TypeMetaNone typeVar2 typeVar3) (TypeTuple TypeMetaNone typeVar (TypeTuple TypeMetaNone typeVar2 typeVar3)))

inferUnaryOperation :: (AST r t) => UnaryOp -> Infer r t Type
inferUnaryOperation OpHead = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone  (TypeList TypeMetaNone typeVar) (typeVar)
inferUnaryOperation OpTails = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone  (TypeList TypeMetaNone typeVar) (TypeList TypeMetaNone typeVar)
inferUnaryOperation OpEmptyList = do
  typeVar  <- freshTypeVar
  typeVar2 <- freshTypeVar
  return $ TypeArrow TypeMetaNone typeVar (TypeList TypeMetaNone typeVar2)
inferUnaryOperation OpEmptyTuple = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone typeVar (TypeTuple TypeMetaNone (TypeUnit TypeMetaNone) (TypeUnit TypeMetaNone))
inferUnaryOperation OpListNth = do
  typeVar <- freshTypeVar
  return $ TypeArrow TypeMetaNone (TypeList TypeMetaNone typeVar) typeVar
inferUnaryOperation (OpTupleNth index len) = do
  (tupleType, elsTypes) <- foldrM
    (\_ (tup, typeVars) -> do
      typeVar <- freshTypeVar
      return $ ((TypeTuple TypeMetaNone typeVar tup), [typeVar] ++ typeVars)
    )
    ((TypeTuple TypeMetaNone (TypeUnit TypeMetaNone) (TypeUnit TypeMetaNone)), [])
    (replicate len 0)
  return $ TypeArrow TypeMetaNone (tupleType) (elsTypes !! index)
