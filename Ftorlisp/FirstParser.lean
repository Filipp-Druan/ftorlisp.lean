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
deriving Repr

def intParser : Parser FirstParserError ParseTree := do
  let minus ← maybe (char '-')
  let num ← wholeNumber
  match minus with
    | .some _ => return (.int (- (Int.ofNat num)))
    | .none => return .int $ Int.ofNat num

def isMathChar (ch : Char) : Bool :=
  ch == '+' || ch == '-' || ch == '*' || ch == '/'

def isSymbolChar (ch : Char) : Bool :=
  ch.isAlphanum || isMathChar ch

def symParser : Parser FirstParserError ParseTree := do
  let chars ← many1 (sat isSymbolChar)
  return .sym (String.ofList chars.toList)

mutual
  partial def listParser : Parser FirstParserError ParseTree := do
    let _ ←  char '('
    let exprs ← sepBy exprParser ws
    let _ ← char ')'
    return .call exprs.toList

  partial def exprParser : Parser FirstParserError ParseTree := do
    (withErr (.custom FirstParserError.int)  intParser) <|>
    (withErr (.custom FirstParserError.sym)  symParser) <|>
    (withErr (.custom FirstParserError.list) listParser)
end

#eval (exprParser (ParserState.mk "(+ ()+)" 0))
