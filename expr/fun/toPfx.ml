open Ast
open FunPfx.Ast

(*

FIRST VERION OF generate FOR QUESTION 10

(* Function which increments all tracked variable index by 1 *)
let increment_all var_pos =
  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value + 1)
  ) var_pos ;;

(* Function which decrements all tracked variable index by 1 *)
let decrement_all var_pos =
  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value - 1)
  ) var_pos ;;


(* Translates an Expr AST to a list of Pfx commands *)
let generate expr =  

  (* Hashmap tracking all variable value's position in the stack *)
  let var_pos = Hashtbl.create 1 in

  (* Main function *)
  let rec aux var_pos = function
  
  (* An element is added to the stack so we need to update index of all variables in the stack *)
  | Const v -> increment_all var_pos; [PUSH(v)]

  (* Overall, an element is removed from the stack when any binary operation is performed.
     As such, we need to update the index of all variables in the stack *)
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
  
  (* Adding then performing a Binop cancel each other in terms of stack position *)     
  | Uminus v -> (aux var_pos v)@[PUSH(0); SUB]
  
   (* Unbound variable *)
  | Var var_name  when (Hashtbl.find_opt var_pos var_name) == None ->  raise (Failure ("Unbound variable: " ^ var_name))

  (* When we meet a variable, we fetch its value from the hashmap position *)
  | Var var_name -> let v = Hashtbl.find var_pos var_name in 
                    let _ = increment_all var_pos in
                    [PUSH(v); GET]
  
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux var_pos arg_expr in
              
                      Hashtbl.replace var_pos var_name 0;
              
                      let body_code = aux var_pos (Fun(var_name, body)) in
              
                      arg_code @ body_code @ [EXEC; SWAP; POP]                                  
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(_, v) -> [EXEC_SEQ(aux var_pos v)]
  
  in aux var_pos expr;;

  *)

(* Function which increments all tracked variable index by 1 *)
let increment_all var_pos =
  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value + 1)
  ) var_pos ;;

(* Function which decrements all tracked variable index by 1 *)
let decrement_all var_pos =
  Hashtbl.iter (fun key value ->
    Hashtbl.replace var_pos key (value - 1)
  ) var_pos ;;

(* Returns a list of all free variables from an Expr expression *)
let rec get_free_variable bound_var = function
  | Var(name) when (List.mem name bound_var) -> []
  | Var(name) -> [name]
  | Binop(_, v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | Uminus(v) -> get_free_variable bound_var v
  | Fun(name, v) -> let new_bound_var = name::bound_var in (get_free_variable new_bound_var v)
  | App(v1, v2) -> (get_free_variable bound_var v1) @ (get_free_variable bound_var v2) 
  | _ -> []

(* Returns a list of commands to add all free variables to a function via the APPEND Pfx command*)
let add_free_var var_pos free_var =
  let rec aux_add_free_var var_pos = function
  |[] -> []
  |var_name::t ->
            let pos = Hashtbl.find var_pos var_name in 
            Hashtbl.replace var_pos var_name ( (List.length t));
            [PUSH(pos + (List.length free_var)); GET; APPEND] @ (aux_add_free_var var_pos t)
  in aux_add_free_var var_pos free_var;;

(* Returns a list of commands to remove all formerly added free variables via APPEND *)
let rec clean_free_var var_pos = function
  |[] -> []
  |_::t -> decrement_all var_pos ; SWAP::POP::(clean_free_var var_pos t);;

(* Translates an Expr AST to a list of Pfx commands *)
let generate expr =  

  (* Hashmap tracking all variable value's position in the stack *)
  let var_pos = Hashtbl.create 1 in

  (* List tracking all free variables *)
  let free_var = get_free_variable [] expr in

  (* Main function *)
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
  
  (* Unbound variable *)
  | Var var_name  when (Hashtbl.find_opt var_pos var_name) == None ->  raise (Failure ("Unbound variable: " ^ var_name))

  (* When we meet a variable, we fetch its value from the hashmap position *)
  | Var var_name -> let v = Hashtbl.find var_pos var_name in 
                    let _ = increment_all var_pos in
                    [PUSH(v); GET]

  (* If variable name is already used*)
  | App(Fun(var_name, _), _) when (Hashtbl.find_opt var_pos var_name) <> None ->  raise (Failure ("Variable name already used"))

  (* First compute the right side. Then starts tracking variable and compute the function  *)
  | App(Fun(var_name, body), arg_expr) ->
                      let arg_code = aux var_pos free_var arg_expr in
                      Hashtbl.replace var_pos var_name 0; (* Track new variable*)
                      let new_free_var = get_free_variable [var_name] body  in (* Get the free variables of the function body*)
                      let body_code = aux var_pos new_free_var (Fun(var_name, body)) in
                      arg_code @ body_code @ [EXEC] @ (clean_free_var var_pos new_free_var) @ [SWAP; POP]                           

  (* For failing cases, return an error *)
  | App(_, _) -> failwith "Left term of application must be a function"

  | Fun(var_name, v) -> let bound_var_pos = Hashtbl.find var_pos var_name in 
                        Hashtbl.replace var_pos var_name ((List.length free_var) + bound_var_pos);
                        let new_var_pos = Hashtbl.copy var_pos in 
                        let append_cmds = add_free_var new_var_pos free_var in
                        [EXEC_SEQ(aux new_var_pos free_var v)] @ (append_cmds)

  
  in aux var_pos free_var expr;;
