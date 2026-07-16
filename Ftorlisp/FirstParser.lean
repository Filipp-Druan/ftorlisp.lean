-- В этом файле находится первый парсер, который переводит код в S-выражения,
-- Которые потом переводятся в абстрактное синтаксическое дерево.

import Ftorlisp.ParserCombinators
import Ftorlisp.ParseTree
open Ftorlisp.ParserCombinators
open Ftorlisp.ParseTree

namespace Ftorlisp.FirstParser

inductive FirstParserError where
  | number
  | sym
  | list
deriving Inhabited, Repr

def combineToFloat (whole : Int) (frac : Nat) : Float :=
  -- Определяем количество цифр в дробной части
  let len := (toString frac).length
  -- Возводим 10 в степень количества цифр
  let divisor := (10 : Nat) ^ len

  -- Переводим части в Float и делим
  let fPart := Float.ofInt (frac : Int) / Float.ofInt (divisor : Int)

  -- Если целая часть отрицательная, дробную нужно вычитать
  if whole < 0 then
    Float.ofInt whole - fPart
  else
    Float.ofInt whole + fPart

-- Пример использования:
#eval combineToFloat 24 256   -- Результат: 2.250000
#eval combineToFloat (-2) 25 -- Результат: -2.250000

private def numberParser : Parser FirstParserError ParseTree := do
  let minus ← maybe (char '-')
  let num ← wholeNumber
  let dot ← maybe $ char '.'

  let int_part := match minus with
    | .some _ => - Int.ofNat num
    | .none => num

  match dot with
    | .some _ => do
      let num_end ← wholeNumber
      return .number $ combineToFloat int_part num_end
    | .none => return .number $ Float.ofInt int_part


private def isMathChar (ch : Char) : Bool :=
  ch ∈ ['+', '-', '*', '/', '=']

private def isSymbolChar (ch : Char) : Bool :=
  ch.isAlphanum || isMathChar ch

private def symParser : Parser FirstParserError ParseTree := do
  let chars ← many1 (sat isSymbolChar)
  return .sym (String.ofList chars.toList)

private def isOpenBracket (char : Char) : Bool :=
  char ∈ ['(', '[']

private def isCloseBracket (char : Char) : Bool :=
  char ∈ [')', ']']

mutual
  private partial def listParser : Parser FirstParserError ParseTree := do
    let _ ←  sat isOpenBracket
    let exprs ← sepBy exprParser ws
    let _ ← sat isCloseBracket
    return .call exprs.toList

  private partial def exprParser : Parser FirstParserError ParseTree := do
    (withErr (.custom .number)  numberParser) <|>
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
  let arr ← sepBy exprParser (many ws)
  return arr.toList

partial def programFirstParser (src : String) : Except FirstParserError (List ParseTree) :=
  match programParser ⟨src, 0⟩ with
    | .error err => match err.err with
      | .custom fperr => .error fperr
      | _ => unreachable!
    | .ok parser_res => .ok parser_res.val

#eval (exprFirstParser "(+ 1 2 (* 3 4))")
