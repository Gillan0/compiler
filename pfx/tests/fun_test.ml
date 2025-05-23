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

let _ = compile "./fun_prog.pfx";;

(* Test #1 : Expected result 3*)
let cmds = [PUSH(1); PUSH(5); SWAP; POP; PUSH(8); ADD; EXEC_SEQ([PUSH(2);PUSH(1);ADD]); EXEC];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #2 : Expected result 5*)
let cmds = [PUSH(5); PUSH(7); PUSH(1); GET];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #2 : Expected result 7*)
let cmds = [PUSH(5); PUSH(7); PUSH(0); GET];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #4 : Expected result 8*)
let cmds = [EXEC_SEQ([PUSH(3); ADD]); PUSH(5); APPEND; EXEC];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #5 : Expected result 13*)
let cmds = [EXEC_SEQ([PUSH(3); ADD]); EXEC_SEQ([PUSH(5); PUSH(2); MUL]); APPEND; EXEC];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #6 : Expected result ERROR*)
let cmds = [PUSH(5); EXEC_SEQ([PUSH(5); PUSH(2); MUL]); APPEND; EXEC];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;