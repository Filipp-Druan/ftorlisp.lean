import Ftorlisp.Ty
import Ftorlisp.OpTypes

open Ftorlisp.OpTypes
open Ftorlisp.Ty

namespace Ftorlisp.TyAST
mutual
  inductive TyASTExpr where
    | number (ty : Ty) (val : Float)
    | bool (ty : Ty) (val : Bool)
    | binOp (ty : Ty) (op : BinOp) (arg1 arg2 : TyASTExpr)
    | unOp (ty : Ty) (op : UnOp) (arg : TyASTExpr)
    | varRead (ty : Ty) (name : String)
    | if_expr (ty : Ty) (test : TyASTExpr) (then_exp : TyASTExpr) (else_exp : TyASTExpr)
    | eq (ty : Ty) (args : List TyASTExpr)
    | fn_expr (ty : Ty) (list : List TyASTExpr)
  deriving Inhabited, BEq

  inductive TyASTStmt where
    | let_stmt (ty : Ty) (name : String) (val : TyASTExpr)
    | dec (ty : Ty) (name : String)
  deriving BEq

  inductive TyAST where
    | exp (val : TyASTExpr)
    | stmt (val : TyASTStmt)
  deriving BEq

end

namespace TyASTExpr
  def ty (ast: TyASTExpr) : Ty :=
    match ast with
      | .number ty _ => ty
      | .bool ty _ => ty
      | .varRead ty _ => ty
      | .binOp ty _ _ _ => ty
      | .unOp ty _ _ => ty
      | .eq ty _ => ty
      | .if_expr ty _ _ _ => ty
      | .fn_expr ty _ => ty

end TyASTExpr

namespace Ftorlisp.TyASTPrinter

-- Вспомогательная функция для генерации нужного количества пробелов
def spaces (n : Nat) : String :=
  String.ofList (List.replicate n ' ')

mutual
  -- Обработка выражений
  partial def exprToString (ast : TyASTExpr) (ind : Nat := 0) : String :=
    match ast with
    | .number ty val =>
      s!"{val} : {Ty.tyToString ty}"
    | .bool ty val =>
      s!"{val} : {Ty.tyToString ty}"
    | .varRead ty name =>
      s!"{name} : {Ty.tyToString ty}"
    | .unOp ty op arg =>
      let opS := match op with | .neg => "-"
      -- Длина префикса: "(" + "op" + " " (например, "(- " это 3 символа)
      let indNext := ind + 2 + opS.length
      s!"({opS} {exprToString arg indNext}) : {Ty.tyToString ty}"
    | .binOp ty op arg1 arg2 =>
      let opS := match op with
        | .add => "+"
        | .sub => "-"
        | .mul => "*"
        | .div => "/"
      -- Длина префикса: "(" + "op" + " "
      let indNext := ind + 2 + opS.length
      s!"({opS} {exprToString arg1 indNext}\n{spaces indNext}{exprToString arg2 indNext}) : {Ty.tyToString ty}"
    | .if_expr ty test then_exp else_exp =>
      -- Длина префикса: "(if " (4 символа)
      let indNext := ind + 4
      s!"(if {exprToString test indNext}\n{spaces indNext}{exprToString then_exp indNext}\n{spaces indNext}{exprToString else_exp indNext}) : {Ty.tyToString ty}"

    | .eq ty args =>
      -- Длина префикса: "(eq " (4 символа)
      let indNext := ind + 4
      let argsStrs := args.map (fun e => exprToString e indNext)
      let body := String.intercalate s!"\n{spaces indNext}" argsStrs
      s!"(eq {body}) : {Ty.tyToString ty}"
    | .fn_expr ty list =>
      -- Для вызова функции отступ для аргументов равен 2 символам (сразу под именем функции, учитывая "(" )
      let indNext := ind + 2
      let argsStrs := list.map (fun e => exprToString e indNext)
      let body := String.intercalate s!"\n{spaces indNext}" argsStrs
      s!"({body}) : {Ty.tyToString ty}"
    end

  -- Обработка утверждений (statements)
  partial def stmtToString (ast : TyASTStmt) (ind : Nat := 0) : String :=
    match ast with
    | .let_stmt _ty name val =>
      -- Длина префикса: "(let " (5) + имя переменной + " " (1)
      let indNext := ind + 6 + name.length
      s!"(let {name} {exprToString val indNext})"
    | .dec ty name =>
      -- Для dec нам нужно разобрать тип функции, чтобы получить список аргументов и возвращаемый тип
      match ty with
      | .fn arg_tys ret_ty =>
        let argsStr := "[" ++ String.intercalate " " (arg_tys.map Ty.tyToString) ++ "]"
        s!"(dec {name} {argsStr} {Ty.tyToString ret_ty})"
      | _ =>
        -- Fallback, если dec по какой-то причине имеет не функциональный тип
        s!"(dec {name} : {Ty.tyToString ty})"


  -- Главная функция для перевода всего узла AST
  partial def astToString (ast : TyAST) (ind : Nat := 0) : String :=
    match ast with
    | .exp e => exprToString e ind
    | .stmt s => stmtToString s ind

end Ftorlisp.TyASTPrinter

instance : Repr TyAST where
    reprPrec ast _ :=
      Repr.reprPrec  (Ftorlisp.TyASTPrinter.astToString ast) 0

instance : Repr TyASTExpr where
    reprPrec ast _ :=
      Repr.reprPrec  (Ftorlisp.TyASTPrinter.exprToString ast) 0

instance : Repr TyASTStmt where
    reprPrec ast _ :=
      Repr.reprPrec  (Ftorlisp.TyASTPrinter.stmtToString ast) 0
