(* Define stack type for function support (int & exec_seq can be in the stack) *)
type stack_type = 
  | Int of int
  | EXEC_SEQ of Ast.command list 

val eval_program: Ast.program -> stack_type list -> unit