open FunPfx.Lexer
open FunPfx.Eval
open FunPfx.Ast

let rec examine_all lexbuf =
  let result = token lexbuf in
  print_token result;
  print_string " ";
  match result with
    | EOF -> ()
    | _ -> examine_all lexbuf  

let compile file =
  print_string ("fun_test : File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ = compile "fun_prog.pfx";;

(* Test #1 : Expected result 3*)
let cmds = [PUSH(1); PUSH(5); SWAP; POP; PUSH(8); ADD; EXEC_SEQ([PUSH(2);PUSH(1);ADD]); EXEC];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;