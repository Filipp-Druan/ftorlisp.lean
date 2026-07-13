import Ftorlisp.UnTyAST
import Ftorlisp.OpTypes
import Std.Data.HashMap

open Ftorlisp.OpTypes
open Ftorlisp.UnTyAST
open Std (HashMap)

namespace Ftorlisp.TyInference

inductive Ty where
  | int
  | bool
deriving Inhabited, Repr, BEq


structure TyTable where
  map : HashMap String Ty
deriving Repr, BEq

namespace TyTable
  def init : TyTable :=
    let map : HashMap String Ty := ∅
    let full_map := map.insertMany [
      ("Int", .int),
      ("Bool", .bool)
    ]
    ⟨full_map⟩

  def lookup (ty_table : TyTable) (name : String) : Option Ty :=
    ty_table.map.get? name

  def int (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Int").get!

  def bool (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Bool").get!

  def isInt (ty_table : TyTable) (ty : Ty) : Bool :=
    ty_table.int == ty
end TyTable

structure VarTyEnv where
  parent : Option VarTyEnv
  scope : HashMap String Ty
deriving Repr, BEq

namespace VarTyEnv

  def init : VarTyEnv :=
    {parent := .none, scope := .emptyWithCapacity}

  def lookup (env : VarTyEnv) (name : String) : Option Ty :=
    env.scope.get? name

  def insert (env : VarTyEnv) (name : String) (ty : Ty) : VarTyEnv :=
    { env with scope := env.scope.insert name ty}

end VarTyEnv

mutual
  inductive TyASTExpr where
    | int (ty : Ty) (val : Int)
    | bool (ty : Ty) (val : Bool)
    | binOp (ty : Ty) (op : BinOp) (arg1 arg2 : TyASTExpr)
    | unOp (ty : Ty) (op : UnOp) (arg : TyASTExpr)
    | varRead (ty : Ty) (name : String)
    | if_expr (ty : Ty) (test : TyASTExpr) (then_exp : TyASTExpr) (else_exp : TyASTExpr)
  deriving Inhabited, Repr

  inductive TyASTStmt where
    | let_stmt (ty : Ty) (name : String) (val : TyASTExpr)
  deriving Repr, BEq

  inductive TyAST where
    | exp (val : TyASTExpr)
    | stmt (val : TyASTStmt)
  deriving Repr, BEq

  inductive TyInfError where
  | undefinedVar
  | arithArgsTypeMismatch (arg1 arg2 : TyASTExpr)
  | arithNoArgs
  | negNotNum (arg : TyASTExpr)
  | ifTypeMissmatch (then_ast : TyASTExpr) (else_ast : TyASTExpr)
  | ifConditionNotBool (test : TyASTExpr)
  deriving Repr, BEq

end

namespace TyASTExpr
  def ty (ast: TyASTExpr) : Ty :=
    match ast with
      | .int ty _ => ty
      | .bool ty _ => ty
      | .varRead ty _ => ty
      | .binOp ty _ _ _ => ty
      | .unOp ty _ _ => ty
      | .if_expr ty _ _ _ => ty
end TyASTExpr

abbrev TyInfExcept := Except TyInfError


inductive TyInfRes (α : Type) where
  | envUpdate (env : VarTyEnv)
  | ty (ty : α)
deriving Repr, BEq

mutual
  private partial def expTyInference (exp : UnTyASTExpr) (ty_table : TyTable) (env : VarTyEnv) : TyInfExcept TyASTExpr :=
    match exp with
      | .intLit val => .ok $ .int ty_table.int val
      | .bool val => .ok $ .bool ty_table.bool val
      | .sym name => match (env.lookup name) with
        | .some ty => .ok $ .varRead ty name
        | .none => .error .undefinedVar

      | .binOp op arg1 arg2 => do
        let arg1_ast ← expTyInference arg1 ty_table env
        let arg2_ast ← expTyInference arg2 ty_table env

        if arg1_ast.ty == arg2_ast.ty && arg1_ast.ty == ty_table.int then
          return .binOp arg1_ast.ty op arg1_ast arg2_ast
        else
          .error $ .arithArgsTypeMismatch arg1_ast arg2_ast

      | .unOp .neg arg => do
        let arg_ast ← expTyInference arg ty_table env

        if arg_ast.ty == ty_table.int then
          return (.unOp arg_ast.ty .neg arg_ast)
        else
          .error $ .negNotNum arg_ast

      | .if_expr test then_exp else_exp => do
        let test_ast ← expTyInference test ty_table env
        let then_ast ← expTyInference then_exp ty_table env
        let else_ast ← expTyInference else_exp ty_table env

        match test_ast.ty with
          | .bool =>
            if then_ast.ty == else_ast.ty then
              return .if_expr then_ast.ty test_ast then_ast else_ast
            else
              .error $ .ifTypeMissmatch then_ast else_ast
          | _ => .error $ .ifConditionNotBool test_ast

  private partial def stmtTyInference (stmt : UnTyASTStmt) (ty_table : TyTable) (env : VarTyEnv) : TyInfExcept TyASTStmt := do
    match stmt with
      | .let_stmt name val => do
        let val_ast ← expTyInference val ty_table env
        return (.let_stmt val_ast.ty name val_ast)

  partial def astTyInference (ast : UnTyAST) (ty_table : TyTable) (env : VarTyEnv) : TyInfExcept TyAST := do
    match ast with
      | .expr exp => do
        let exp_typed ← expTyInference exp ty_table env
        return .exp exp_typed
      | .stmt stmt => do
        let stmt_typed ← stmtTyInference stmt ty_table env
        return .stmt stmt_typed
end


partial def programTyInference (ast_list : List UnTyAST) (ty_table : TyTable) (env : VarTyEnv) : TyInfExcept $ (List TyAST × VarTyEnv) := do
  match ast_list with
    | [] => return ([], env)
    | ast :: rest => do
      let tyast ← astTyInference ast ty_table env
      match tyast with
        | .exp _ => do
          let (rest_tyast, rest_env) ← (programTyInference rest ty_table env)
          return (tyast :: rest_tyast, rest_env)
        | .stmt stmt => do
          match stmt with
            | .let_stmt ty name _ => do
              let new_env := env.insert name ty
              let (rest_tyast, rest_env) ← (programTyInference rest ty_table new_env)
              return (tyast :: rest_tyast, rest_env)
