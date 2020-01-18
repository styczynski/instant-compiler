module SSA.Syntax where

data Data =
  Void
  | Var Int
  | ConstStr String
  | ConstInt Int
  | ConstBool Bool

data Reg = Reg Int

data Instr =
  Add Reg Data Data
  | Div Reg Data Data
  | Mod Reg Data Data
  | Sub Reg Data Data
  | Call Int [Data]
  | If Data [Instr] [Instr]
  | While Data [Instr] [Instr]
  | Return Data
  | Label Int

data TopLevelDecl =
  Func Int String [Instr]
  | Main [Instr]

data Program = Program [TopLevelDecl]