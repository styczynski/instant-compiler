{-# language DataKinds #-}
module X86.Compiler.Compiler where

import Syntax.Base
import X86.X86
import Data.Int

type X86 m = CodeM LCode CodeLine m

cExpr :: (WithTypedSize s) => Operand AccessReadWrite s -> Expr ASTMetadata -> X86 ()
cExpr out (ELitInt _ val) = do
  mov out $ ImmOp $ Immediate $ fromIntegral val
cExpr out (ELitTrue _) = mov out $ ImmOp $ Immediate $ fromIntegral 1
cExpr out (ELitFalse _) = mov out $ ImmOp $ Immediate $ fromIntegral 0
cExpr out (EAdd _ expr1 (Plus _) expr2) = do
  cExpr out expr1
  temp <- allocReg
  cExpr temp expr2
  add out $ resizeOperand temp
cExpr out (EAdd _ expr1 (Minus _) expr2) = do
  cExpr out expr1
  temp <- allocReg
  cExpr temp expr2
  sub out $ resizeOperand temp
cExpr out (EMul _ expr1 (Div _) expr2) = do
  cExpr out expr1
  temp <- allocReg
  cExpr temp expr2
  idiv out $ resizeOperand temp
cExpr out (EMul _ expr1 (Times _) expr2) = do
  cExpr out expr1
  temp <- allocReg
  cExpr temp expr2
  imul out $ resizeOperand temp
cExpr _ _ = return ()

cDecl :: Item ASTMetadata -> X86 ()
cDecl (Init _ (Ident name) expr) = do
  r <- allocReg
  cExpr r expr
cDecl _ = return ()

cStmt :: Stmt ASTMetadata -> X86 ()
cStmt (Ass _ (Ident name) expr) = do
  r <- allocReg
  cExpr r expr
cStmt (Decl _ _ items) = do
  mapM_ cDecl items
cStmt _ = return ()

cFunc :: String -> [Stmt ASTMetadata] -> X86 ()
cFunc _ body = do
  mapM_ cStmt body

cTopDef :: TopDef ASTMetadata -> X86 ()
cTopDef (FnDef _ _ (Ident name) _ (Block _ stmts)) = do
  f <- return $ cFunc name stmts
  declareFunction name [] f

cProgram :: Program ASTMetadata -> X86 ()
cProgram program@(Program _ statements) = do
  mapM_ cTopDef statements
  r1 <- allocReg
  mov r1 r1

compileX86 p = show $ withLabels $ cProgram p
