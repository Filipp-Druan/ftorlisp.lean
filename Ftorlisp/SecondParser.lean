import Ftorlisp.FirstParser
import Ftorlisp.ParseTree
import Ftorlisp.UnTyAST
import Ftorlisp.OpTypes

open Ftorlisp.UnTyAST
open Ftorlisp.FirstParser
open Ftorlisp.ParseTree
open Ftorlisp.OpTypes

/-
Данный файл содержит код второго парсера,
который переводит S-выражения в нетипизированное
AST.
-/

namespace Ftorlisp.SecondParser

inductive SecondParserError where
  | unknownOperator (parse_tree : ParseTree)
  | operatorNotSymbol (parse_tree : ParseTree)
  | notCallUnreachable
  | notArgsUnreachable
  | notStmtServis --
  | notExpServis -- Служебные ошибки, используются для управления парсингом
  | notExp (parse_tree : ParseTree)
  | notExpAndNotStmt
  | foldBinOpArgsEmptyList
  | emptyCall
  | arithNot2Args
  | letNot2Args (args : List ParseTree)
  | letValNotExp (ast : UnTyAST)
  | letNameNotSym
  | ifNot3Args (args : List ParseTree)
  | tyConsNotSym (parse_tree : ParseTree)
  | tyNameIsSpecial
  | tyBadExpr (parse_tree : ParseTree)
  | decEmpty
  | decArgsAndRetNo (args : List ParseTree)
  | decRetNo (args : List ParseTree)
  | decArgsNotList (args : List ParseTree)
  | decFunNameNotSym (args : List ParseTree)
  | decToMachDecArgs (args : List ParseTree)
  | fnCallIncorrectOpertor (parse_tree : ParseTree)
deriving Inhabited, Nonempty, Repr, BEq

abbrev SPExcept := Except SecondParserError

partial def isSpecialName (name : String) : Bool :=
  name ∈ ["+", "-", "*", "/", "let", "if", "dec"]

mutual
  private partial def exprParser (parse_tree : ParseTree) : SPExcept UnTyASTExpr :=
    match parse_tree with
      | .int val => .ok $ .intLit val
      | .sym name => match name with
        | "true" => .ok $ .bool true
        | "false" => .ok $ .bool false
        | _ => .ok $ .sym name
      | .call (oper :: args) =>
        match oper with
          | .sym "+" => binOpParser .add args
          | .sym "*" => binOpParser .mul args
          | .sym "-" => minusParser args
          | .sym "/" => binOpParser .div args
          | .sym "if" => ifParser args
          | .sym name =>
            if isSpecialName name then
              .error .notExpServis
            else
              fnParser parse_tree
          | .call _ => fnParser parse_tree
          | _ => .error $ .fnCallIncorrectOpertor parse_tree
      | .call [] => .error .emptyCall

  private partial def fnParser
    (parse_tree : ParseTree) : SPExcept UnTyASTExpr := do
    match parse_tree with
      | .call list => do
        let ast_list ← list.mapM exprParser
        return .fn_call ast_list
      | _ => unreachable!


  private partial def tyParser(parse_tree : ParseTree) : SPExcept UnTyASTTy := do
    match parse_tree with
      | .sym name => .ok $ .sym name
      | .call (head :: tail) => match head with
        | .sym cons_ty_name => do
          if isSpecialName cons_ty_name then
            .error .tyNameIsSpecial
          let arg_tys ← tail.mapM tyParser
          return UnTyASTTy.call cons_ty_name arg_tys
        | _ => .error $ .tyConsNotSym head
      | _ => .error $ .tyBadExpr parse_tree

  private partial def decParser (args : List ParseTree) : SPExcept UnTyASTStmt := do
    match args with
      | [] => .error .decEmpty
      | [_name] => .error $ .decArgsAndRetNo args
      | [_name, _arg_tys] => .error $ .decRetNo args
      | [name_tree, arg_tys, ret_ty] => match name_tree with
        | .sym fun_name => do
          match arg_tys with
            | .call list => do
              let arg_tys_asts ← list.mapM tyParser
              let ret_ty_ast ← tyParser ret_ty
              return .dec fun_name arg_tys_asts ret_ty_ast
            | _ => .error $ .decArgsNotList args
        | _ => .error $ .decFunNameNotSym args
      | _ => .error $ .decToMachDecArgs args



  private partial def ifParser (args : List ParseTree) : SPExcept UnTyASTExpr := do
    match args with
      | [] => .error .emptyCall
      | [_] => .error $ .ifNot3Args args
      | [_, _,] => .error $ .ifNot3Args args
      | [test, then_exp, else_exp] => do
        let test_ast ← exprParser test
        let then_ast ← exprParser then_exp
        let else_ast ← exprParser else_exp

        return .if_expr test_ast then_ast else_ast
      | _ => .error $ .ifNot3Args args

  private partial def foldBinOpArgs (oper : BinOp) (first_ast : UnTyASTExpr) (rest : List ParseTree) : SPExcept UnTyASTExpr :=
    match rest with
      | [second] => do
        let second_ast ← exprParser second
        return (.binOp oper first_ast second_ast)
      | second :: rest_rest => do
        let second_ast ← exprParser second
        (foldBinOpArgs oper (.binOp oper first_ast second_ast) rest_rest)
      | [] => .error .foldBinOpArgsEmptyList
  private partial def binOpParser (oper : BinOp) (args : List ParseTree) : SPExcept UnTyASTExpr := do
    match args with
      | [] => Except.error SecondParserError.emptyCall
      | [_] => .error .arithNot2Args
      | [arg1, arg2] => do
        let arg1_ast ← exprParser arg1
        let arg2_ast ← exprParser arg2
        return (UnTyASTExpr.binOp  oper arg1_ast arg2_ast)
      | arg1 :: rest => do
        let arg1_ast ← exprParser arg1
        foldBinOpArgs oper arg1_ast rest

  private partial def minusParser (args : List ParseTree) : SPExcept UnTyASTExpr :=
    match args with
      | [] => .error .emptyCall
      | [arg] => do
        let arg_ast ← exprParser arg
        return .unOp .neg arg_ast
      | [arg1, arg2] => do
        let arg1_ast ← exprParser arg1
        let arg2_ast ← exprParser arg2
        return .binOp .sub arg1_ast arg2_ast
      | arg1 :: rest => do
          let arg1_ast ← exprParser arg1
          let rest_ast ← (binOpParser .sub rest)
          return .binOp .sub arg1_ast rest_ast


  private partial def letStmtParser (args : List ParseTree) : SPExcept UnTyASTStmt :=
    match args with
      | [name, val] => do
        let name_ast ← exprParser name
        let val_ast ← exprParser val

        match name_ast with
          | .sym name_str => .ok $ .let_stmt name_str val_ast
          | _ => .error .letNameNotSym

      | _ => .error $ .letNot2Args args

  private partial def stmtParser (parse_tree : ParseTree) : SPExcept UnTyASTStmt :=
    match parse_tree with
      | .call (oper :: args) =>
        match oper with
          | .sym "let" => letStmtParser args
          | .sym "dec" => decParser args
          | .sym _ => .error $ .unknownOperator oper
          | _ => .error $ .operatorNotSymbol oper
      | _ => .error .notStmtServis

  partial def astSecondParser (parse_tree : ParseTree) : SPExcept UnTyAST :=
    let exp_res := exprParser parse_tree
    match exp_res with
      | Except.ok exp_ast => .ok $ .expr exp_ast
      | .error .notExpServis =>
        let stmt_res := stmtParser parse_tree
        match stmt_res with
          | .ok stmt_ast => .ok $ .stmt stmt_ast
          | .error .notStmtServis => .error .notExpAndNotStmt
          | .error err => .error err
      | .error err => .error err

  partial def programSecondParser (prog_pares_tree : List ParseTree) : SPExcept $ List UnTyAST := do
    let prog_ast ← prog_pares_tree.mapM astSecondParser
    return prog_ast
end

#eval do
  let pt ← (exprFirstParser "(let num (+ 1 2 3))")
  return astSecondParser pt

#eval do
  let pt ← (exprFirstParser "(let boo true)")
  return astSecondParser pt

#eval do
  let pt ← (exprFirstParser "(dec foo [Bool] Bool)")
  return astSecondParser pt
