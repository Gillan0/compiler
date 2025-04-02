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
|EXEC_SEQ of command list 
|EXEC
|GET of int ;;

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
