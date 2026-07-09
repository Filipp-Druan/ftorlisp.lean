namespace Ftorlisp.UnTyAST

mutual
  inductive UnTyExprAST where
  | add (list : List UnTyAST)
  | mul (list : List UnTyAST)
  | sub (list : List UnTyAST)
  | div (list : List UnTyAST)
  deriving Nonempty, Repr, BEq

  inductive UnTyAST where
  | intLit (val : Int)
  | sym (name : String)
  | exp (val : UnTyExprAST)
  | let_statement (name : UnTyAST) (val : UnTyAST) -- name : UnTyAST.sym
  deriving Nonempty, Repr, BEq
end
