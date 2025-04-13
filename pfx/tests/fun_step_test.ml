open FunPfx.Eval
open FunPfx.Ast

let _ = Printf.printf "\n\n\n## Starting test for step in fun Pfx\n"

(* Test #1 : Expected result 13*)
let cmds = [PUSH(1); PUSH(5); SWAP; POP; PUSH(8); ADD];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #2 : Expected result 1*)
let cmds = [PUSH(5); PUSH(6); REM];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #3 : Expected result 2*)
let cmds = [PUSH(5); PUSH(12); DIV];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #4 : Expected result 60*)
let cmds = [PUSH(5); PUSH(12); MUL];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #5 : Expected result  No result*)
let cmds = [];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #6 : Expected result  Error*)
let cmds = [POP];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #7 : Expected result  Error*)
let cmds = [MUL];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #8 : Expected result  Error*)
let cmds = [REM];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #9 : Expected result Error*)
let cmds = [PUSH(0); PUSH(12); DIV];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;

(* Test #9 : Expected result Error*)
let cmds = [PUSH(0); PUSH(12); REM];;
let state = (0,  cmds);;
let _ = eval_program (state) [];;


let _ = Printf.printf "## Ending test for step in fun Pfx\n\n\n"