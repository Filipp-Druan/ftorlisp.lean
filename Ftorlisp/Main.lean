import Ftorlisp.FirstParser
import Ftorlisp.SecondParser
import Ftorlisp.TyInference

open Ftorlisp.FirstParser
open Ftorlisp.SecondParser
open Ftorlisp.TyInference

inductive GeneralError where
  | firstParserError (err :  FirstParserError)
  | secondParserError (err : SecondParserError)
  | tyInfError (err : TyInfError)
deriving Repr

#eval do
  let env := Environment.init
  let ty_table := TyTable.init

  let pt ← exprFirstParser "123" |> Except.mapError GeneralError.firstParserError
  let utast ← astSecondParser pt |> Except.mapError GeneralError.secondParserError
  let tyast ← astTyInference utast ty_table env |> Except.mapError GeneralError.tyInfError

  return tyast


partial def srcToTyAST (src : String) : Except GeneralError $ (List TyAST × Environment) := do
  let env := Environment.init
  let ty_table := TyTable.init

  let pt ← programFirstParser src |> Except.mapError GeneralError.firstParserError
  let utast ← programSecondParser pt |> Except.mapError GeneralError.secondParserError
  let tyast ← programTyInference utast ty_table env |> Except.mapError GeneralError.tyInfError

  return tyast
