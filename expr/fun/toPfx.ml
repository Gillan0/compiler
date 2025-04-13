open Ast
open FunPfx.Ast

(*

FIRST VERION OF generate FOR QUESTION 10

let increment_all var_pos =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value + 1)
  ) var_pos ;;

  increment_all (Hashtbl.create 1)

let decrement_all var_pos =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value - 1)
  ) var_pos ;;

  decrement_all (Hashtbl.create 1)

let generate expr =  

  let var_pos = Hashtbl.create 1 in

  let rec aux var_pos = function
  
  | Const v -> increment_all var_pos; [PUSH(v)]

  | Binop(Badd,v1,v2) -> let prio_term = aux var_pos v2 in
                        let last_term = aux var_pos v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[ADD]

  | Binop(Bsub,v1,v2) -> let prio_term = aux var_pos v2 in
                        let last_term = aux var_pos v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[SUB]
                        
  | Binop(Bmul,v1,v2) -> let prio_term = aux var_pos v2 in
                        let last_term = aux var_pos v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[MUL]

  | Binop(Bdiv,v1,v2) -> let prio_term = aux var_pos v2 in
                        let last_term = aux var_pos v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[DIV]
          
  | Binop(Bmod,v1,v2) -> let prio_term = aux var_pos v2 in
                        let last_term = aux var_pos v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[REM]
  
                        
  | Uminus v -> (aux var_pos v)@[PUSH(0); SUB]
  
  | Var var_name -> (try 
                      let v = Hashtbl.find var_pos var_name in 
                      let _ = increment_all var_pos in
                      [PUSH(v); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux var_pos arg_expr in
              
                      Hashtbl.replace var_pos var_name 0;
              
                      let body_code = aux var_pos (Fun(var_name, body)) in
              
                      arg_code @ body_code @ [EXEC; SWAP; POP]                                  
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(_, v) -> [EXEC_SEQ(aux var_pos v)]
  
  in aux var_pos expr;;

  *)


let increment_all var_pos =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value + 1)
  ) var_pos ;;

let decrement_all var_pos =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value - 1)
  ) var_pos ;;

let rec get_free_variable bound_var = function
  | Var(name) when (List.mem name bound_var) -> []
  | Var(name) -> [name]
  | Binop(_, v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | Uminus(v) -> get_free_variable bound_var v
  | Fun(name, v) -> let new_bound_var = name::bound_var in (get_free_variable new_bound_var v)
  | App(v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | _ -> []

let add_free_var var_pos free_var =
  let rec aux_add_free_var var_pos = function
  |[] -> []
  |var::t -> 
            let pos = Hashtbl.find var_pos var in 
            Hashtbl.replace var_pos var (pos + (List.length free_var)) ;
            [PUSH(pos); GET; APPEND] @ (aux_add_free_var var_pos t)
  in aux_add_free_var var_pos free_var;;

let rec clean_free_var var_pos = function
  |[] -> []
  |_::t -> decrement_all var_pos ; SWAP::POP::(clean_free_var var_pos t);;

let generate expr =  

  (* Hashmap tracking all variable value's position in the stack *)
  let var_pos = Hashtbl.create 1 in

  (* List tracking all free variables *)
  let free_var = get_free_variable [] expr in

  let rec aux var_pos free_var = function
  (* An element is added to the stack so we need to update index of all variables in the stack *)
  | Const v -> increment_all var_pos; [PUSH(v)]

  (* Overall, an element is removed from the stack when any binary operation is performed.
     As such, we need to update the index of all variables in the stack *)
  | Binop(Badd,v1,v2) -> let prio_term = aux var_pos free_var v2 in
                        let last_term = aux var_pos free_var v1 in 
                        let _ = decrement_all var_pos in 
                        (prio_term)@(last_term)@[ADD]

  | Binop(Bsub,v1,v2) -> let prio_term = aux var_pos free_var v2 in
                        let last_term = aux var_pos free_var v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[SUB]
                        
  | Binop(Bmul,v1,v2) -> let prio_term = aux var_pos free_var v2 in
                        let last_term = aux var_pos free_var v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[MUL]

  | Binop(Bdiv,v1,v2) -> let prio_term = aux var_pos free_var v2 in
                        let last_term = aux var_pos free_var v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[DIV]
          
  | Binop(Bmod,v1,v2) -> let prio_term = aux var_pos free_var v2 in
                        let last_term = aux var_pos free_var v1 in 
                        let _ = decrement_all var_pos in
                        (prio_term)@(last_term)@[REM]
  
  (* Adding then performing a Binop cancel each other in terms of stack position *)
  | Uminus v -> (aux var_pos free_var v)@[PUSH(0); SUB]
  
  (* When we meet a variable, we fetch its value from the hashmap position *)
  | Var var_name -> (try 
                      let v = Hashtbl.find var_pos var_name in 
                      let _ = increment_all var_pos in
                      [PUSH(v); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux var_pos free_var arg_expr in
                      Hashtbl.replace var_pos var_name 0;
                      let body_code = aux var_pos free_var (Fun(var_name, body)) in
                      arg_code @ body_code @ [EXEC] @ (clean_free_var var_pos free_var) @ [SWAP; POP]                           
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(var_name, v) -> 
                        let new_free_var = get_free_variable [var_name] v  in 
                        let bound_var_pos = Hashtbl.find var_pos var_name in 
                        Hashtbl.replace var_pos var_name ((List.length new_free_var) + bound_var_pos);
                        let new_var_pos = Hashtbl.copy var_pos in 
                        let append_cmds = add_free_var new_var_pos new_free_var in
                        [EXEC_SEQ(aux new_var_pos new_free_var v)] @ (append_cmds)
  
  in aux var_pos free_var expr;;
