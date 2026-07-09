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

                return .plus arg_ASTs
              | "*" => do
                let arg_ASTs ← List.mapM ast_parser arguments

                return .mul arg_ASTs
              | _ => .error (.unknownOperator operator)
          | _ => .error (.operatorNotSymbol operator)
      | _ => .error .notCallUnreachable
end
