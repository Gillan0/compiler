open Ast
open FunPfx.Ast

(*

FIRST VERION OF generate FOR QUESTION 10

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

  *)


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

let rec get_free_variable bound_var = function
  | Var(name) when (List.mem name bound_var) -> []
  | Var(name) -> [name]
  | Binop(_, v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | Uminus(v) -> get_free_variable bound_var v
  | Fun(name, v) -> let new_bound_var = name::bound_var in (get_free_variable new_bound_var v)
  | App(v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | _ -> []

let rec add_free_var p = function
  |[] -> []
  |var::t -> increment_all p ;
            let pos = Hashtbl.find p var in 
            [PUSH(pos); GET; APPEND] @ (add_free_var p t);;

let rec clean_free_var p = function
  |[] -> []
  |_::t -> decrement_all p ; SWAP::POP::(clean_free_var p t);;

let generate expr =  

  let p = Hashtbl.create 1 in

  let free_var = get_free_variable [] expr in

  let rec aux p free_var = function
  
  | Const v -> increment_all p; [PUSH(v)]

  | Binop(Badd,v1,v2) -> let prio_term = aux p free_var v2 in
                        let last_term = aux p free_var v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[ADD]

  | Binop(Bsub,v1,v2) -> let prio_term = aux p free_var v2 in
                        let last_term = aux p free_var v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[SUB]
                        
  | Binop(Bmul,v1,v2) -> let prio_term = aux p free_var v2 in
                        let last_term = aux p free_var v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[MUL]

  | Binop(Bdiv,v1,v2) -> let prio_term = aux p free_var v2 in
                        let last_term = aux p free_var v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[DIV]
          
  | Binop(Bmod,v1,v2) -> let prio_term = aux p free_var v2 in
                        let last_term = aux p free_var v1 in 
                        let _ = decrement_all p in
                        (prio_term)@(last_term)@[REM]
  
                        
  | Uminus v -> (aux p free_var v)@[PUSH(0); SUB]
  
  | Var var_name when (List.mem var_name free_var) -> (try 
                      let v = Hashtbl.find p var_name in 
                      let _ = increment_all p in
                      [PUSH(v); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))

  | Var var_name -> (try 
                      let v = Hashtbl.find p var_name in 
                      let _ = increment_all p in
                      [PUSH(v); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux p free_var arg_expr in
                      Hashtbl.replace p var_name 0;
                      let body_code = aux p free_var (Fun(var_name, body)) in
                      arg_code @ body_code @ [EXEC] @ (clean_free_var p free_var) @ [SWAP; POP]                           
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(var_name, v) -> let new_free_var = get_free_variable [var_name] v  in 
                        [EXEC_SEQ(aux p new_free_var v)] @ (add_free_var p new_free_var)
  
  in aux p free_var expr;;
