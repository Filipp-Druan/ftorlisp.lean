import Ftorlisp.OpTypes

open Ftorlisp.OpTypes

namespace Ftorlisp.UnTyAST

mutual
  inductive UnTyASTExpr where
    | intLit (val : Int)
    | sym (name : String)
    | binOp (op : BinOp) (arg1 arg2 : UnTyASTExpr)
    | unOp (op : UnOp) (arg : UnTyASTExpr)
  deriving Nonempty, Repr, BEq

  inductive UnTyASTStmt where
    | let_stmt (name : String) (val : UnTyASTExpr) -- name - всегда .sym

  inductive UnTyAST where
    | exp (val : UnTyASTExpr)
    | stmt (val : UnTyASTStmt)
  deriving Nonempty, Repr, BEq
end

namespace UnTyAST

def toInt (ast : UnTyAST) : Option Int :=
    match ast with
      | .exp (.intLit val) => .some val
      | _ => .none

#guard (UnTyAST.exp (.intLit 10)).toInt == .some 10

end UnTyAST
end Ftorlisp.UnTyAST
