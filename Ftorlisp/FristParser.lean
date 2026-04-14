import Ftorlisp.ParseTree

open Ftorlisp
open ParseTree

abbrev ParserPos := Nat


structure ParserState where
  input : String
  pos : ParserPos
deriving Repr, BEq

structure ParserError (ε : Type) where
  err : ε
  pos : ParserPos

inductive StandardParserError where
  | mismatch
  | endOfInput
  | conversionFail

def Parser (ε α : Type) := ParserState → Except (ParserError ε) (α × ParserState)

def ppure (val : α): Parser ε α :=
  λ state => .ok (val, state)

def pbind (pars : Parser ε α) (fn : α -> Parser ε β): Parser ε β :=
  λ state => match pars state with
    | .error err => .error err
    | .ok (val, state) => (fn val) state

def pfail (err : ε) : Parser ε α :=
  λ state => .error { err := err, pos := state.pos }

-- Превращает Option в Parser. Если .none, падает с указанной ошибкой.
def fromOption (opt : Option α) (err : ε) : Parser ε α :=
  match opt with
  | .some val => ppure val
  | .none     => pfail err

instance : Monad (Parser ε) where
  pure := ppure
  bind := pbind


def sat (pred : Char → Bool) : Parser StandardParserError Char :=
  λ state =>
    let str := state.input
    if (str.isEmpty) then
      let pos := state.pos
      .error { err := .endOfInput, pos := pos}
    else
      if pred (str.front) then
        let new_input := (str.drop 1).toString
        let new_pos := state.pos + 1
        .ok (str.front, { input := new_input, pos := new_pos })
      else
        .error { err := .mismatch, pos := state.pos}

partial def manyCore (parser : Parser ε α) (acc : Array α): Parser ε (Array α) :=
  λ state =>
    match parser state with
      | .error _ => .ok (acc, state)
      | .ok (val, new_state) =>
        if new_state.input = state.input then
          .error ⟨(panic! "Парсер не уменьшает строку в комбинаторе many!"), new_state.pos⟩
        else
          manyCore parser (acc.push val) new_state

partial def many (parser : Parser ε α): Parser ε (Array α) :=
  λ state => (manyCore parser #[]) state

instance : OrElse (Parser ε α) where
  orElse parser_1 parser_2 :=  λ state =>
    let res := parser_1 state
    match res with
      | .ok val => .ok val
      | .error _ => parser_2 () state

instance : Functor (Parser ε) where
  map f parser := λ state =>
    let res := parser state
    match res with
      | .ok (val, new_state) => .ok (f val, new_state)
      | .error err => .error err

def char (ch : Char) : Parser StandardParserError Char :=
  sat (· = ch)

def string (pattern : String) : Parser StandardParserError String :=
  λ state =>
    if (state.input.startsWith pattern) then
      let rest := (state.input.drop (pattern.length)).toString
      .ok (pattern, ⟨rest, state.pos + pattern.length⟩)
    else
      .error ⟨.mismatch, state.pos ⟩

def ws : Parser StandardParserError Char :=
  sat (·.isWhitespace)


def digitToNat (ch : Char): Option Nat :=
  let code := ch.toNat
  if (47 < code) && (code < 58) then
    .some (code - 48)
  else
    .none

def digit : Parser StandardParserError Nat := do
  let ch ← sat Char.isDigit
  fromOption (digitToNat ch) .conversionFail

#guard match (digit ⟨"1", 0⟩) with
  | .ok (num, _) => num = 1
  | .error _ => false

-- Этот комбинатор парсит набор цифр, которые идут подряд друг за другом,
-- без разделения при помощи знака _
def wholeNumber : Parser StandardParserError Nat := do
  let digits ← many digit
  let num := digits.foldl (fun acc num => acc * 10 + num) 0
  return num

#guard match (wholeNumber ⟨"123", 0⟩) with
  | .ok (num, _) => num = 123
  | .error _ => false
