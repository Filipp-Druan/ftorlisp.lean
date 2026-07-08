namespace Ftorlisp.UnTyAST

inductive UnTyAST where
  | intLit (val : Int)
  | sym (name : String)
  | plus (list : List UnTyAST)
  | mul (list : List UnTyAST)
deriving Nonempty, Repr, BEq
