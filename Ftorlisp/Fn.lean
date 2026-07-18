import Ftorlisp.Ty
import Ftorlisp.TyAST

open Ftorlisp.Ty
open Ftorlisp.TyAST

namespace Ftorlisp.Fn

structure FnDef where
  ast : List TyAST
deriving Repr, BEq

inductive FnError where
  | fnDefined
deriving Repr, BEq

structure Fn where
  ty : Ty
  definition : Option FnDef
deriving Repr, BEq

namespace Fn
  def makeFromDecTy (dec_ty : Ty) : Fn :=
    ⟨dec_ty, .none⟩

  def addDef (fn : Fn) (fn_def : FnDef) : Except FnError Fn :=
    match fn.definition with
      | .some _ => .error .fnDefined
      | _ => .ok {fn with definition := .some fn_def}
end Fn
