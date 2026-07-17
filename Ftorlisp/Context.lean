import Std.Data.HashMap
import Ftorlisp.Ty
import Ftorlisp.Fn

open Std (HashMap)
open Ftorlisp.Ty
open Ftorlisp.Fn

namespace Ftorlisp.Context

private structure TyTable where
  map : HashMap String Ty
deriving Repr, BEq

namespace TyTable
  def init : TyTable :=
    let map : HashMap String Ty := ∅
    let full_map := map.insertMany [
      ("Number", .number),
      ("Bool", .bool)
    ]
    ⟨full_map⟩

  def lookup (ty_table : TyTable) (name : String) : Option Ty :=
    ty_table.map.get? name

  def number (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Number").get!

  def bool (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Bool").get!

  def isInt (ty_table : TyTable) (ty : Ty) : Bool :=
    ty_table.number == ty
end TyTable

private structure VarTyEnv where
  scope : HashMap String Ty
deriving Repr, BEq

namespace VarTyEnv

  def init : VarTyEnv :=
    {scope := .emptyWithCapacity}

  def lookup (env : VarTyEnv) (name : String) : Option Ty :=
    env.scope.get? name

  def insert (env : VarTyEnv) (name : String) (ty : Ty) : VarTyEnv :=
    { env with scope := env.scope.insert name ty}

end VarTyEnv

structure Context where
  parent : Option Context
  fn_table : HashMap String Fn
  var_ty_env : VarTyEnv
  ty_table : TyTable
deriving Repr, BEq

namespace Context
  def init : Context :=
    ⟨.none, ∅, .init, .init⟩

  partial def varTyLookup (context : Context) (name : String) : Option Ty :=
    match context.var_ty_env.lookup name with
      | .some ty => .some ty
      | .none => do
        let par ← context.parent
        par.varTyLookup name

  def varTyInsert (context : Context) (name : String) (ty : Ty) : (Context × Bool) :=
    match context.var_ty_env.lookup name with
      | .none => ({context with var_ty_env := context.var_ty_env.insert name ty}, true)
      | .some _ => (context, false)

  def tyLookup (context : Context) (name : String) : Option Ty :=
    match context.ty_table.lookup name with
      | .some ty => .some ty
      | .none => do
        let par ← context.parent
        par.tyLookup name

  def fnInsert (context : Context) (name : String) (fn : Fn) : (Context × Bool) :=
    let isIn := name ∈ context.fn_table
    if isIn then
      (context, false)
    else
      ({context with
        fn_table   := context.fn_table.insert   name fn,
        var_ty_env := context.var_ty_env.insert name fn.ty},
       true)

  def fnAddDef (context : Context) (name : String) (fn_def : FnDef) : (Context × Bool) :=
    let opt_fn := context.fn_table[name]?
    match opt_fn with
      | .some fn =>
        let new_fn := fn.addDef fn_def
        let new_context := {context with fn_table := context.fn_table.insert name new_fn}
        (new_context, true)
      | .none =>
        (context, false)

  def tyNumber (context : Context) : Ty :=
    context.ty_table.number

  def tyBool (context : Context) : Ty :=
    context.ty_table.bool
end Context
