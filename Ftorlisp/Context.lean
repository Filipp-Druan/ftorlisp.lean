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
      ("Int", .int),
      ("Bool", .bool)
    ]
    ⟨full_map⟩

  def lookup (ty_table : TyTable) (name : String) : Option Ty :=
    ty_table.map.get? name

  def int (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Int").get!

  def bool (ty_table : TyTable) : Ty :=
    (ty_table.lookup "Bool").get!

  def isInt (ty_table : TyTable) (ty : Ty) : Bool :=
    ty_table.int == ty
end TyTable

private structure VarTyEnv where
  parent : Option VarTyEnv
  scope : HashMap String Ty
deriving Repr, BEq

namespace VarTyEnv

  def init : VarTyEnv :=
    {parent := .none, scope := .emptyWithCapacity}

  def lookup (env : VarTyEnv) (name : String) : Option Ty :=
    env.scope.get? name

  def insert (env : VarTyEnv) (name : String) (ty : Ty) : VarTyEnv :=
    { env with scope := env.scope.insert name ty}

end VarTyEnv

structure Context where
  fn_table : HashMap String Fn
  var_ty_env : VarTyEnv
  ty_table : TyTable
deriving Repr, BEq

namespace Context
  def init : Context :=
    ⟨∅, .init, .init⟩

  def varTyLookup (context : Context) (name : String) : Option Ty :=
    context.var_ty_env.lookup name

  def varTyInsert (context : Context) (name : String) (ty : Ty) : Context :=
    {context with var_ty_env := context.var_ty_env.insert name ty}

  def tyLookup (context : Context) (name : String) : Option Ty :=
    context.ty_table.lookup name

  def fnInsert (context : Context) (name : String) (fn : Fn) : (Context × Bool) :=
    let isIn := name ∈ context.fn_table
    if isIn then
      (context, false)
    else
      ({context with fn_table := context.fn_table.insert name fn}, true)

  def fnAddDef (context : Context) (name : String) (fn_def : FnDef) : (Context × Bool) :=
    let opt_fn := context.fn_table[name]?
    match opt_fn with
      | .some fn =>
        let new_fn := fn.addDef fn_def
        let new_context := {context with fn_table := context.fn_table.insert name new_fn}
        (new_context, true)
      | .none =>
        (context, false)

  def tyInt (context : Context) : Ty :=
    context.ty_table.int

  def tyBool (context : Context) : Ty :=
    context.ty_table.bool
end Context
