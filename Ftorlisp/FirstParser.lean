-- В этом файле находится первый парсер, который переводит код в S-выражения,
-- Которые потом переводятся в абстрактное синтаксическое дерево.

import Ftorlisp.ParserCombinators
import Ftorlisp.ParseTree
open Ftorlisp.ParserCombinators
open Ftorlisp.ParseTree

namespace Ftorlisp.FirstParser

def intParser : Parser StandardParserError ParseTree := do
  let minus ← maybe (char '-')
  let num ← wholeNumber
  match minus with
    | .some _ => return (.int (- (Int.ofNat num)))
    | .none => return .int $ Int.ofNat num

def isMathChar (ch : Char) : Bool :=
  ch == '+' || ch == '-' || ch == '*' || ch == '/'

def isSymbolChar (ch : Char) : Bool :=
  ch.isAlphanum || isMathChar ch

def symParser : Parser StandardParserError ParseTree := do
  let chars ← many (sat isSymbolChar)
  return .sym (String.ofList chars.toList)

mutual
  partial def listParser : Parser StandardParserError ParseTree := do
    let exprs ← many exprParser
    return .call exprs.toList

  partial def exprParser : Parser StandardParserError ParseTree := do
    intParser <|> symParser <|> listParser
end
