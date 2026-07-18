namespace Ftorlisp.ParseTree

inductive ParseTree where
  | number (val : Float)
  | sym (name : String)
  | string (val : String)
  | call (list : List ParseTree)
deriving Nonempty, Repr, BEq
