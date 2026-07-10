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
  | letNot2Args (args : List ParseTree)
  | letValNotExp (ast : UnTyAST)
  | letNameNotSym
deriving Nonempty, Repr, BEq

mutual
  partial def ast_parser (parse_tree : ParseTree) : Except SecondParserError UnTyAST :=
    match parse_tree with
      | .int val => .ok (.intLit val)
      | .sym name => .ok (.sym name)
      | .call _ => (call_parser parse_tree)

  partial def call_parser (parse_tree : ParseTree) : Except SecondParserError UnTyAST :=
    match parse_tree with
      | .call (operator :: arguments) =>
        match operator with
          | .sym name =>
            match name with
              | "+" => do
                let arg_ASTs ← List.mapM ast_parser arguments

                return .exp $ .add arg_ASTs
              | "*" => do
                let arg_ASTs ← List.mapM ast_parser arguments

                return .exp $ .mul arg_ASTs
              | "-" => do
                let arg_ASTs ← List.mapM ast_parser arguments

                return .exp $ .sub arg_ASTs
              | "/" => do
                let arg_ASTs ← List.mapM ast_parser arguments

                return .exp $ .div arg_ASTs

              | "let" => let_args_parser arguments

              | _ => .error (.unknownOperator operator)
          | _ => .error (.operatorNotSymbol operator)
      | _ => .error .notCallUnreachable

  partial def let_name_parser (name : ParseTree) : Except SecondParserError UnTyAST := do
    let name_ast ← ast_parser name

    match name_ast with
      | .sym _ => return name_ast
      | _ => .error .letNameNotSym

  partial def let_val_parser (val : ParseTree) : Except SecondParserError UnTyAST := do
    let val_ast ← ast_parser val

    match val_ast with
      | .exp _ => return val_ast
      | _ => .error $ .letValNotExp val_ast

  partial def let_args_parser (args : List ParseTree) : Except SecondParserError UnTyAST :=
    match args with
      | [name, val] => do
        let name_ast ← let_name_parser name
        let val_ast ← let_val_parser val

        return .let_statement name_ast val_ast
      | _ => .error $ .letNot2Args args
end

#eval do
  let pt ← (exprParser ⟨"(+ 1 2)", 0⟩)
  return ast_parser pt.val
