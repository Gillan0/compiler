open Ast
open FunPfx.Ast

let increment_all p =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace p key (value + 1)
  ) p ;;

  increment_all (Hashtbl.create 1)

let decrement_all p =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace p key (value - 1)
  ) p ;;

  decrement_all (Hashtbl.create 1)

let generate expr =  

  let p = Hashtbl.create 1 in

  let rec aux p = function
  
  | Const v -> increment_all p; [PUSH(v)]

  | Binop(Badd,v1,v2) -> let prio_term = aux p v2 in
                        let last_term = aux p v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[ADD]

  | Binop(Bsub,v1,v2) -> let prio_term = aux p v2 in
                        let last_term = aux p v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[SUB]
                        
  | Binop(Bmul,v1,v2) -> let prio_term = aux p v2 in
                        let last_term = aux p v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[MUL]

  | Binop(Bdiv,v1,v2) -> let prio_term = aux p v2 in
                        let last_term = aux p v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[DIV]
          
  | Binop(Bmod,v1,v2) -> let prio_term = aux p v2 in
                        let last_term = aux p v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[REM]
  
                        
  | Uminus v -> (aux p v)@[PUSH(0); SUB]
  
  | Var var_name -> (try 
                      let v = Hashtbl.find p var_name in 
                      let _ = increment_all p in
                      [PUSH(v); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux p arg_expr in
              
                      Hashtbl.replace p var_name 0;
              
                      let body_code = aux p (Fun(var_name, body)) in
              
                      arg_code @ body_code @ [EXEC; SWAP; POP]                                  
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(_, v) -> [EXEC_SEQ(aux p v)]
  
  in aux p expr;;