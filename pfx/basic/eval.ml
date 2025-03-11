open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

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
  | PUSH v :: q , stack -> Ok (q, v::stack)
  | POP :: q , v::stack -> Ok (q, stack)
  | SWAP :: q , v1::v2::stack -> Ok (q, v2::v1::stack)
  | ADD :: q , v1::v2::stack -> Ok (q, (v1 + v2)::stack)
  | SUB :: q, v1::v2::stack -> Ok (q, (v1 - v2)::stack)
  | MUL :: q, v1::v2::stack -> Ok (q, (v1 * v2)::stack)
  | DIV :: q, v1::v2::stack -> Ok (q, (v1 / v2)::stack)
  | REM :: q, v1::v2::stack -> Ok (q, (v1 mod v2)::stack)
  (* Invalid configurations *)
  | _ :: q , v::[] -> Error("Runtime Error",state)
  | _ :: q , [] -> Error("Runtime Error",state);;

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
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"