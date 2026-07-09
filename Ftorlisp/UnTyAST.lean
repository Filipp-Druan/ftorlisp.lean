namespace Ftorlisp.UnTyAST

inductive UnTyAST where
  | intLit (val : Int)
  | sym (name : String)
  | add (list : List UnTyAST)
  | mul (list : List UnTyAST)
  | sub (list : List UnTyAST)
  | div (list : List UnTyAST)
deriving Nonempty, Repr, BEq
