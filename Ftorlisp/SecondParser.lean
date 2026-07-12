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

inductive SecondParserError where
  | unknownOperator (parse_tree : ParseTree)
  | operatorNotSymbol (parse_tree : ParseTree)
  | notCallUnreachable
  | notArgsUnreachable
  | notStmtServis --
  | notExpServis -- Служебные ошибки, используются для управления парсингом
  | notExp (parse_tree : ParseTree)
  | notExpAndNotStmt
  | emptyCall
  | arithNot2Args
  | letNot2Args (args : List ParseTree)
  | letValNotExp (ast : UnTyAST)
  | letNameNotSym
deriving Nonempty, Repr, BEq

abbrev SPExcept := Except SecondParserError

mutual
  partial def expParser (parse_tree : ParseTree) : SPExcept UnTyASTExpr :=
    match parse_tree with
      | .int val => .ok $ .intLit val
      | .sym name => .ok $ .sym name
      | .call (oper :: args) =>
        match oper with
          | .sym "+" => binOpParser .add args
          | .sym "*" => binOpParser .mul args
          | .sym "-" => minusParser args
          | .sym "/" => binOpParser .div args
          | _ => .error .notExpServis
      | .call [] => .error .emptyCall

  partial def binOpParser (oper : BinOp) (args : List ParseTree) : SPExcept UnTyASTExpr := do
    match args with
      | [] => Except.error SecondParserError.emptyCall
      | [_] => .error .arithNot2Args
      | [arg1, arg2] => do
        let arg1_ast ← expParser arg1
        let arg2_ast ← expParser arg2
        return (UnTyASTExpr.binOp  oper arg1_ast arg2_ast)
      | arg1 :: rest => do
        let arg1_ast ← expParser arg1
        let rest_ast ← (binOpParser oper rest)
        return .binOp oper arg1_ast rest_ast

  partial def minusParser (args : List ParseTree) : SPExcept UnTyASTExpr :=
    match args with
      | [] => .error .emptyCall
      | [arg] => do
        let arg_ast ← expParser arg
        return .unOp .neg arg_ast
      | [arg1, arg2] => do
        let arg1_ast ← expParser arg1
        let arg2_ast ← expParser arg2
        return .binOp .sub arg1_ast arg2_ast
      | arg1 :: rest => do
          let arg1_ast ← expParser arg1
          let rest_ast ← (binOpParser .sub rest)
          return .binOp .sub arg1_ast rest_ast

  partial def letStmtParser (args : List ParseTree) : SPExcept UnTyASTStmt :=
    match args with
      | [name, val] => do
        let name_ast ← expParser name
        let val_ast ← expParser val

        match name_ast with
          | .sym name_str => .ok $ .let_stmt name_str val_ast
          | _ => .error .letNameNotSym

      | _ => .error $ .letNot2Args args

  partial def stmtParser (parse_tree : ParseTree) : SPExcept UnTyASTStmt :=
    match parse_tree with
      | .call (oper :: args) =>
        match oper with
          | .sym "let" => letStmtParser args
          | .sym _ => .error $ .unknownOperator oper
          | _ => .error $ .operatorNotSymbol oper
      | _ => .error .notStmtServis

  partial def astParser (parse_tree : ParseTree) : SPExcept UnTyAST :=
    let exp_res := expParser parse_tree
    match exp_res with
      | Except.ok exp_ast => .ok $ .exp exp_ast
      | .error .notExpServis =>
        let stmt_res := stmtParser parse_tree
        match stmt_res with
          | .ok stmt_ast => .ok $ .stmt stmt_ast
          | .error .notStmtServis => .error .notExpAndNotStmt
          | .error err => .error err
      | .error err => .error err

end

#eval do
  let pt ← (exprParser ⟨"(let num (+ 1 2 3))", 0⟩)
  return astParser pt.val
