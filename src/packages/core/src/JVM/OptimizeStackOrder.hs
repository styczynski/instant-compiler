module JVM.OptimizeStackOrder where

import Syntax.Base

optimizeStackOrderExp :: Exp -> (Exp, Int)
optimizeStackOrderExp v@(ExpVar _) = (v, 1)
optimizeStackOrderExp v@(ExpLit _) = (v, 1)
optimizeStackOrderExp (ExpAdd l r) = let (ol, dl) = optimizeStackOrderExp l in let (or, dr) = optimizeStackOrderExp r in
  if dl < dr then ((ExpAdd ol or), dr+1) else ((ExpAdd or ol, dl+1))
optimizeStackOrderExp (ExpDiv l r) = let (ol, dl) = optimizeStackOrderExp l in let (or, dr) = optimizeStackOrderExp r in
  if dl < dr then ((ExpDiv ol or), dr+1) else ((ExpDiv or ol, dl+1))
optimizeStackOrderExp (ExpSub l r) = let (ol, dl) = optimizeStackOrderExp l in let (or, dr) = optimizeStackOrderExp r in
  if dl < dr then ((ExpSub ol or), dr+1) else ((ExpSub or ol, dl+1))
optimizeStackOrderExp (ExpMul l r) = let (ol, dl) = optimizeStackOrderExp l in let (or, dr) = optimizeStackOrderExp r in
  if dl < dr then ((ExpMul ol or), dr+1) else ((ExpMul or ol, dl+1))

optimizeStackOrderStmt :: Stmt -> Stmt
optimizeStackOrderStmt (SExp exp) = let (t, _) = optimizeStackOrderExp exp in SExp t
optimizeStackOrderStmt s = s

optimizeStackOrder :: Program -> Program
optimizeStackOrder (Prog stmts) =
  Prog $ map optimizeStackOrderStmt stmts