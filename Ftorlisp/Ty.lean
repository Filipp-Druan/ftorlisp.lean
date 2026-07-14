namespace Ftorlisp.Ty

inductive Ty where
  | int
  | bool
  | generic_cons (name : String) (arg_tys_num : Nat)
  | generic_spec (gen_cons : Ty) (arg_tys : List Ty)
  | fn (arg_tys : List Ty) (ret_ty : Ty)
deriving Inhabited, Repr, BEq
