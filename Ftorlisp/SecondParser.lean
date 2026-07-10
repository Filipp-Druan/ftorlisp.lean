import Ftorlisp.FirstParser
import Ftorlisp.ParseTree
import Ftorlisp.UnTyAST

open Ftorlisp.UnTyAST
open Ftorlisp.FirstParser
open Ftorlisp.ParseTree

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
  | letNot2Args (args : List ParseTree)
  | letValNotExp (ast : UnTyAST)
  | letNameNotSym
deriving Nonempty, Repr, BEq

abbrev SPExcept := Except SecondParserError

mutual
  partial def math_args_parser (args : List ParseTree) : SPExcept (List UnTyASTExpr) := do
    let args_ast ← List.mapM exp_parser args
    return args_ast

  partial def math_parser (oper : List UnTyASTExpr → UnTyASTExpr) (args : List ParseTree) : SPExcept UnTyASTExpr :=
    let args_res := math_args_parser args
      match args_res with
        | .ok list => .ok $ oper list
        | .error err => .error err

partial def exp_parser (parse_tree : ParseTree) : SPExcept UnTyASTExpr :=
    match parse_tree with
      | .int val => .ok $ .intLit val
      | .sym name => .ok $ .sym name
      | .call (oper :: args) =>
        match oper with
          | .sym "+" => math_parser .add args
          | .sym "*" => math_parser .mul args
          | .sym "-" => math_parser .sub args
          | .sym "/" => math_parser .div args
          | _ => .error .notExpServis
      | .call _ => .error .emptyCall

partial def let_stmt_parser (args : List ParseTree) : SPExcept UnTyASTStmt :=
  match args with
    | [name, val] => do
      let name_ast ← exp_parser name
      let val_ast ← exp_parser val

      match name_ast with
        | .sym _ => .ok $ .let_stmt name_ast val_ast
        | _ => .error .letNameNotSym

    | _ => .error $ .letNot2Args args

  partial def stmt_parser (parse_tree : ParseTree) : SPExcept UnTyASTStmt :=
    match parse_tree with
      | .call (oper :: args) =>
        match oper with
          | .sym "let" => let_stmt_parser args
          | .sym _ => .error $ .unknownOperator oper
          | _ => .error $ .operatorNotSymbol oper
      | _ => .error .notStmtServis

  partial def ast_parser (parse_tree : ParseTree) : SPExcept UnTyAST :=
    let exp_res := exp_parser parse_tree
    match exp_res with
      | .ok exp_ast => .ok $ .exp exp_ast
      | .error .notExpServis =>
        let stmt_res := stmt_parser parse_tree
        match stmt_res with
          | .ok stmt_ast => .ok $ .stmt stmt_ast
          | .error .notStmtServis => .error .notExpAndNotStmt
          | .error err => .error err
      | .error err => .error err



end

#eval do
  let pt ← (exprParser ⟨"(+ 1 2)", 0⟩)
  return ast_parser pt.val
