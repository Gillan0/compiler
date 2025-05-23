(* The type of the commands for the stack machine *)
type command =
  |PUSH of int
  |POP
  |SWAP
  |ADD
  |SUB
  |MUL
  |DIV
  |REM
  (* For function support *)
  |EXEC_SEQ of command list 
  |EXEC
  |GET
  (* For closure support *)
  |APPEND;;

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

val string_of_commands : command list -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
