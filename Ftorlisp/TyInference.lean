import Ftorlisp.UnTyAST
import Ftorlisp.TyAST
import Ftorlisp.OpTypes
import Ftorlisp.Ty
import Ftorlisp.Context

open Ftorlisp.OpTypes
open Ftorlisp.UnTyAST
open Ftorlisp.TyAST
open Ftorlisp.Ty
open Ftorlisp.Context

namespace Ftorlisp.TyInference

inductive TyInfError where
  | undefinedVar
  | arithArgsTypeMismatch (arg1 arg2 : TyASTExpr)
  | arithNoArgs
  | negNotNum (arg : TyASTExpr)
  | ifTypeMissmatch (then_ast : TyASTExpr) (else_ast : TyASTExpr)
  | ifConditionNotBool (test : TyASTExpr)
  | eqArgsLess2 (args :  List TyASTExpr)
  | eqTyMismatch (args :  List TyASTExpr)
  | unknownTy (unty_ast : UnTyASTTy)
  | genericArgsNumMismatch (unty_ast : UnTyASTTy) (correct_num : Nat)
  | genericFirstNotCons (unty_ast : UnTyASTTy)
  | decAlreadyDeclared (ty_ast : TyAST)
  | fnTyMismatch (oper : TyASTExpr) (args : List TyASTExpr)
  | fnOperNotFn (oper : TyASTExpr)
  | fnBadArgsNum (oper : TyASTExpr) (args : List TyASTExpr)
deriving Repr, BEq

abbrev TyInfExcept := Except TyInfError

mutual
  private partial def expTyInference
    (exp : UnTyASTExpr) (context : Context) : TyInfExcept TyASTExpr :=
    match exp with
      | .number val => .ok $ .number context.tyNumber val
      | .bool val => .ok $ .bool context.tyBool val
      | .sym name => match (context.varTyLookup name) with
        | .some ty => .ok $ .varRead ty name
        | .none => .error .undefinedVar

      | .binOp op arg1 arg2 => do
        let arg1_ast ← expTyInference arg1 context
        let arg2_ast ← expTyInference arg2 context

        if arg1_ast.ty == arg2_ast.ty && arg1_ast.ty == context.tyNumber then
          return .binOp arg1_ast.ty op arg1_ast arg2_ast
        else
          .error $ .arithArgsTypeMismatch arg1_ast arg2_ast

      | .unOp .neg arg => do
        let arg_ast ← expTyInference arg context

        if arg_ast.ty == context.tyNumber then
          return (.unOp arg_ast.ty .neg arg_ast)
        else
          .error $ .negNotNum arg_ast

      | .if_expr test then_exp else_exp => do
        let test_ast ← expTyInference test context
        let then_ast ← expTyInference then_exp context
        let else_ast ← expTyInference else_exp context

        match test_ast.ty with
          | .bool =>
            if then_ast.ty == else_ast.ty then
              return .if_expr then_ast.ty test_ast then_ast else_ast
            else
              .error $ .ifTypeMissmatch then_ast else_ast
          | _ => .error $ .ifConditionNotBool test_ast
      | .eq args => do
        let args_tyasts ← args.mapM (expTyInference · context)
        match args_tyasts with
          | [] | [_] => .error $ .eqArgsLess2 args_tyasts
          | first :: rest => do
            if rest.all (·.ty == first.ty) then
              return .eq context.tyBool args_tyasts
            else
              .error $ .eqTyMismatch args_tyasts

      | .fn_call (oper :: args) => fnCallTyInferenct oper args context
      | .fn_call [] => unreachable!


  private partial def fnCallTyInferenct
    (oper : UnTyASTExpr)
    (args : List UnTyASTExpr)
    (context : Context): TyInfExcept TyASTExpr := do

    let oper_ty_ast ← (expTyInference oper context)
    let arg_tys_asts ← args.mapM (expTyInference · context)
    let actual_tys := arg_tys_asts.map (·.ty)
    match oper_ty_ast.ty with
      | .fn expexted_tys ret_ty => do
        if expexted_tys.length == actual_tys.length then
          let eq_list := List.zipWith (· == ·) expexted_tys actual_tys
          if eq_list.all (·) then
            return .fn_expr ret_ty (oper_ty_ast :: arg_tys_asts)
          else
            .error $ .fnTyMismatch oper_ty_ast arg_tys_asts
        else
          .error $ .fnBadArgsNum oper_ty_ast arg_tys_asts
      | _ => .error $ .fnOperNotFn oper_ty_ast



  private partial def tyTyInference
    (ty_ast : UnTyASTTy) (context : Context) : TyInfExcept Ty := do
    match ty_ast with
      | .sym name => match context.tyLookup name with
        | .some ty => return ty
        | .none => .error $ .unknownTy ty_ast
      | .call name arg_tys_asts =>
        let opt_ty_cons := context.tyLookup name
        match opt_ty_cons with
          | .some ty_cons => match ty_cons with
            | .generic_cons _name arg_tys_num => do
              if arg_tys_asts.length == arg_tys_num then
                let arg_tys ← arg_tys_asts.mapM (tyTyInference · context)
                return .generic_spec ty_cons arg_tys
              else
                .error $ .genericArgsNumMismatch ty_ast arg_tys_num
            | _ => .error $ .genericFirstNotCons ty_ast
          | .none => .error $ .unknownTy ty_ast

  private partial def stmtTyInference
    (stmt : UnTyASTStmt) (context : Context) : TyInfExcept TyASTStmt := do
    match stmt with
      | .let_stmt name val => do
        let val_ast ← expTyInference val context
        return (.let_stmt val_ast.ty name val_ast)
      | .dec name arg_tys_asts ret_ty_ast => do
        let args_tys ← arg_tys_asts.mapM (tyTyInference · context)
        let ret_ty ← (tyTyInference ret_ty_ast context)
        return .dec (.fn args_tys ret_ty) name


  partial def astTyInference
    (ast : UnTyAST) (context : Context) : TyInfExcept TyAST := do
    match ast with
      | .expr exp => do
        let exp_typed ← expTyInference exp context
        return .exp exp_typed
      | .stmt stmt => do
        let stmt_typed ← stmtTyInference stmt context
        return .stmt stmt_typed
end


partial def programTyInference
  (ast_list : List UnTyAST) (context : Context) : TyInfExcept $ (List TyAST × Context) := do
  match ast_list with
    | [] => return ([], context)
    | ast :: rest => do
      let tyast ← astTyInference ast context
      match tyast with
        | .exp _ => do
          let (rest_tyast, rest_context) ← (programTyInference rest context)

          return (tyast :: rest_tyast, rest_context)
        | .stmt stmt => do
          match stmt with
            | .let_stmt ty name _ => do
              let new_context := context.varTyInsert name ty

              let (rest_tyast, rest_context) ← (programTyInference rest new_context)

              return (tyast :: rest_tyast, rest_context)
            | .dec ty name => do
              let fn := Fn.Fn.makeFromDecTy ty
              let (new_context, success) := context.fnInsert name fn
              if !success then
                .error $ .decAlreadyDeclared tyast
              else
                let (rest_tyast, rest_context) ← (programTyInference rest new_context)
                return (tyast :: rest_tyast, rest_context)
