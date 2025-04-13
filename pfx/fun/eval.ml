open Ast
open Printf

(* Define stack type for function support (int & exec_seq can be in the stack) *)
type stack_type = 
  | Int of int
  | EXEC_SEQ of command list 

let string_of_stack_type = function
  |Int(v) -> string_of_int v 
  |EXEC_SEQ(cmds) -> String.concat " " (List.map string_of_command cmds)

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_stack_type stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | PUSH v :: q , stack -> Ok (q, Int(v)::stack)
  | POP :: q , _::stack -> Ok (q, stack)
  | SWAP :: q , v1::v2::stack -> Ok (q, v2::v1::stack)
  | ADD :: q , Int(v1)::Int(v2)::stack -> Ok (q, Int(v1 + v2)::stack)
  | SUB :: q, Int(v1)::Int(v2)::stack -> Ok (q, Int(v1 - v2)::stack)
  | MUL :: q, Int(v1)::Int(v2)::stack -> Ok (q, Int(v1 * v2)::stack)
  | DIV :: q, Int(v1)::Int(v2)::stack when v2 <> 0 -> Ok (q, Int(v1 / v2)::stack)
  | REM :: q, Int(v1)::Int(v2)::stack when v2 <> 0 -> Ok (q, Int(v1 mod v2)::stack)
  (* For function support *)
  | EXEC_SEQ(q1) :: q2, stack -> Ok (q2, (EXEC_SEQ(q1)) :: stack)
  | EXEC::q2, EXEC_SEQ(q1)::stack -> Ok(q1 @ q2, stack)
  | GET::q, Int(i)::stack when (List.length stack) > i -> let el = (List.nth stack i) in Ok(q, el::stack)
  (* For closure support *)
  | APPEND::q1, Int(i)::EXEC_SEQ(q2)::stack -> Ok(q1, EXEC_SEQ((PUSH(i)::q2))::stack)
  | APPEND::q1, EXEC_SEQ(q2)::EXEC_SEQ(q3)::stack -> Ok(q1, EXEC_SEQ(q2@q3)::stack)
  (* Invalid configurations *)
  | ADD::_, _ -> Error("Runtime Error",state)
  | SUB::_, _ -> Error("Runtime Error",state)
  | MUL::_, _ -> Error("Runtime Error",state)
  | DIV ::_, _::Int(0)::_ -> Error("Division by Zero",state)
  | DIV::_, _ -> Error("Division by Zero",state)
  | REM::_, _::Int(0)::_ -> Error("Runtime Error",state)
  | REM::_, _ -> Error("Runtime Error",state)
  | EXEC::_, _ -> Error("Runtime Error",state)  
  | GET::_, _ -> Error("Runtime Error",state)  
  | APPEND::_, _ -> Error("Runtime Error",state)  
  | _ , _::[] -> Error("Runtime Error",state)
  | _ , [] -> Error("Runtime Error",state);;

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok (Some v) -> printf "Result: %s\n" (string_of_stack_type v)
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"