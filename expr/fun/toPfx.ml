open Ast
open FunPfx.Ast

let p = Hashtbl.create 1;;

let rec generate expr =  
  let aux depth = function
  
  | Const v ->  [PUSH(v)]

  | Binop(Badd,v1,v2) -> (aux depth v2)@(aux depth v1)@[ADD]
  
  | Binop(Bsub,v1,v2) ->(aux depth v2)@(aux depth v1)@[SUB]
  
  | Binop(Bmul,v1,v2) -> (aux depth v2)@(aux depth v1)@[MUL]
  
  | Binop(Bdiv,v1,v2) -> (aux depth v2)@(aux depth v1)@[DIV]
  
  | Binop(Bmod,v1,v2) -> (aux depth v2)@(aux depth v1)@[REM]
  
  | Uminus v -> (aux depth v)@[PUSH(0); SUB]
  
  | Var var_name -> (try 
                      [PUSH (Hashtbl.find p var_name); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, v2), v3) -> (aux depth v3)@(aux (depth+1) Fun(var_name, v2))@[EXEC]
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(var_name, v) -> Hashtbl.add p var_name (depth+1); 
                        [EXEC_SEQ(aux (depth+1) v)]
  
  in aux 0 expr;;