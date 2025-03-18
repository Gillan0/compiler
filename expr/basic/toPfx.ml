open Ast
open BasicPfx.Ast

let rec generate = function
  | Const v ->  [PUSH(v)]
  | Binop(Badd,v1,v2) -> generate(v2)@generate(v1)@[ADD]
  | Binop(Bsub,v1,v2) -> generate(v2)@generate(v1)@[SUB]
  | Binop(Bmul,v1,v2) -> generate(v2)@generate(v1)@[MUL]
  | Binop(Bdiv,v1,v2) -> generate(v2)@generate(v1)@[DIV]
  | Binop(Bmod,v1,v2) -> generate(v2)@generate(v1)@[REM]
  | Uminus v -> generate(v)@[PUSH(0); SUB]
  | Var _ -> failwith "Not yet supported";;
