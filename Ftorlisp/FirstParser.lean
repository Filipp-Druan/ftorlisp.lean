-- В этом файле находится первый парсер, который переводит код в S-выражения,
-- Которые потом переводятся в абстрактное синтаксическое дерево.

import Ftorlisp.ParserCombinators
import Ftorlisp.ParseTree
open Ftorlisp.ParserCombinators
open Ftorlisp.ParseTree

namespace Ftorlisp.FirstParser

inductive FirstParserError where
  | int
  | sym
  | list
deriving Inhabited, Repr

private def intParser : Parser FirstParserError ParseTree := do
  let minus ← maybe (char '-')
  let num ← wholeNumber
  match minus with
    | .some _ => return (.int (- (Int.ofNat num)))
    | .none => return .int $ Int.ofNat num

private def isMathChar (ch : Char) : Bool :=
  ch == '+' || ch == '-' || ch == '*' || ch == '/'

private def isSymbolChar (ch : Char) : Bool :=
  ch.isAlphanum || isMathChar ch

private def symParser : Parser FirstParserError ParseTree := do
  let chars ← many1 (sat isSymbolChar)
  return .sym (String.ofList chars.toList)

mutual
  private partial def listParser : Parser FirstParserError ParseTree := do
    let _ ←  char '('
    let exprs ← sepBy exprParser ws
    let _ ← char ')'
    return .call exprs.toList

  private partial def exprParser : Parser FirstParserError ParseTree := do
    (withErr (.custom FirstParserError.int)  intParser) <|>
    (withErr (.custom FirstParserError.sym)  symParser) <|>
    (withErr (.custom FirstParserError.list) listParser)
end

partial def exprFirstParser (src : String) : Except FirstParserError ParseTree :=
  match exprParser ⟨src, 0⟩ with
    | .error err => match err.err with
      | .custom fperr => .error fperr
      | _ => unreachable!
    | .ok parser_res => .ok parser_res.val

partial def programParser : Parser FirstParserError (List ParseTree) := do
  let arr ← sepBy exprParser (maybe ws)
  return arr.toList

partial def programFirstParser (src : String) : Except FirstParserError (List ParseTree) :=
  match programParser ⟨src, 0⟩ with
    | .error err => match err.err with
      | .custom fperr => .error fperr
      | _ => unreachable!
    | .ok parser_res => .ok parser_res.val

#eval (exprFirstParser "(+ 1 2 (* 3 4))")
