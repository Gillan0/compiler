open Ast
open FunPfx.Ast
(*
let print_hashtable htbl =
  Hashtbl.iter (fun key value ->
    Printf.printf "Key: %s, Value: %d\n" key value
  ) htbl;;
*)
let increment_all p =

  Hashtbl.iter (fun key value ->
    Hashtbl.replace p key (value + 1)
  ) p ;;


let generate expr =  

  let p = Hashtbl.create 1 in

  let rec aux depth i = function
  
  | Const v -> increment_all p ; [PUSH(v)]

  | Binop(Badd,v1,v2) -> let prio_term = aux depth (i+1) v2 in
                        let last_term = aux depth (i+1) v1 in 
                        (prio_term)@(last_term)@[ADD]

  | Binop(Bsub,v1,v2) -> let prio_term = aux depth (i+1) v2 in
                        let last_term = aux depth (i+1) v1 in 
                        (prio_term)@(last_term)@[SUB]
                        
  | Binop(Bmul,v1,v2) -> let prio_term = aux depth (i+1) v2 in
                        let last_term = aux depth (i+1) v1 in 
                        (prio_term)@(last_term)@[MUL]

  | Binop(Bdiv,v1,v2) -> let prio_term = aux depth (i+1) v2 in
                        let last_term = aux depth (i+1) v1 in 
                        (prio_term)@(last_term)@[DIV]
          
  | Binop(Bmod,v1,v2) -> let prio_term = aux depth (i+1) v2 in
                        let last_term = aux depth (i+1) v1 in 
                        (prio_term)@(last_term)@[REM]
  
                        
  | Uminus v -> (aux depth (i+1) v)@[PUSH(0); SUB]
  
  | Var var_name -> (try 
                      increment_all p;
                      let n = Hashtbl.find p var_name in 
                      Hashtbl.replace p var_name (n-1);
                      [PUSH (Hashtbl.find p var_name); GET]
                    with Not_found -> failwith ("Unbound variable: " ^ var_name))
  
  | App(Fun(var_name, v2), v3) -> let prio_term = aux (depth + 1) (i+1) v3 in
                                  let last_term = aux (depth) (i+1) (Fun(var_name, v2)) in 
                                  prio_term@last_term@[EXEC; SWAP; POP] 
                                  
  
  | App(_, _) -> failwith "Left term of application must be a function"
  
  | Fun(var_name, v) -> Hashtbl.add p var_name (depth); 
                        [EXEC_SEQ(aux (depth) (i+1) v)]
  
  in aux 0 0 expr;;