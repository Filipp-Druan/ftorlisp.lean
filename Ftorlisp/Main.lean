import Ftorlisp.FirstParser
import Ftorlisp.SecondParser
import Ftorlisp.TyInference
import Ftorlisp.Context
import Ftorlisp.TyAST

open Ftorlisp.FirstParser
open Ftorlisp.SecondParser
open Ftorlisp.TyInference
open Ftorlisp.Context
open Ftorlisp.TyAST

inductive GeneralError where
  | firstParserError (err :  FirstParserError)
  | secondParserError (err : SecondParserError)
  | tyInfError (err : TyInfError)
deriving Repr

partial def srcToTyAST (src : String) (context : Context) : Except GeneralError $ (List TyAST × Context) := do

  let pt ← programFirstParser src |> Except.mapError GeneralError.firstParserError
  let utast ← programSecondParser pt |> Except.mapError GeneralError.secondParserError
  let tyast ← programTyInference utast context |> Except.mapError GeneralError.tyInfError

  return tyast

#eval srcToTyAST "1 2 3" .init
#eval srcToTyAST "(let num 5) (if (= num 5) (* num num) 0)" .init
#eval srcToTyAST "(dec foo [Number] Number) (foo 5)" .init
#eval srcToTyAST "(dec foo [Number] Number) (foo 5 5)" .init -- Ошибка - неправильное количество аргументов
