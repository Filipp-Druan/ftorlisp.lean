import Ftorlisp.Ty
import Ftorlisp.OpTypes

open Ftorlisp.OpTypes
open Ftorlisp.Ty

namespace Ftorlisp.TyAST
mutual
  inductive TyASTExpr where
    | int (ty : Ty) (val : Int)
    | bool (ty : Ty) (val : Bool)
    | binOp (ty : Ty) (op : BinOp) (arg1 arg2 : TyASTExpr)
    | unOp (ty : Ty) (op : UnOp) (arg : TyASTExpr)
    | varRead (ty : Ty) (name : String)
    | if_expr (ty : Ty) (test : TyASTExpr) (then_exp : TyASTExpr) (else_exp : TyASTExpr)
  deriving Inhabited, Repr, BEq

  inductive TyASTStmt where
    | let_stmt (ty : Ty) (name : String) (val : TyASTExpr)
    | dec (ty : Ty) (name : String)
  deriving Repr, BEq

  inductive TyAST where
    | exp (val : TyASTExpr)
    | stmt (val : TyASTStmt)
  deriving Repr, BEq

end

namespace TyASTExpr
  def ty (ast: TyASTExpr) : Ty :=
    match ast with
      | .int ty _ => ty
      | .bool ty _ => ty
      | .varRead ty _ => ty
      | .binOp ty _ _ _ => ty
      | .unOp ty _ _ => ty
      | .if_expr ty _ _ _ => ty
end TyASTExpr
