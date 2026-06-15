namespace Ftorlisp.ParseTree

inductive ParseTree where
  | int (val : Int)
  | sym (name : String)
  | call (list : List ParseTree)
deriving Nonempty, Repr, BEq
