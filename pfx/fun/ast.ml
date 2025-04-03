type command =
  |PUSH of int
  |POP
  |SWAP
  |ADD
  |SUB
  |MUL
  |DIV
  |REM
  |EXEC_SEQ of command list 
  |EXEC
  |GET;;

type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let string_of_command = function
  |PUSH(a) -> "push"^string_of_int(a)
  |POP -> "pop"
  |SWAP -> "swap"
  |ADD -> "add"
  |SUB -> "sub"
  |MUL -> "mul"
  |DIV -> "div"
  |REM -> "rem"
  |EXEC -> "exec"
  |GET -> "get"
  |EXEC_SEQ(commands) -> "exec_seq"^(List.map string_of_command commands)

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

