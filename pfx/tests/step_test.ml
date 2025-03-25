(* Test #1 : Expected result 13*)
let cmds = [BasicPfx.Ast.PUSH(1); BasicPfx.Ast.PUSH(5); BasicPfx.Ast.SWAP; BasicPfx.Ast.POP; BasicPfx.Ast.PUSH(8); BasicPfx.Ast.ADD];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #2 : Expected result 1*)
let cmds = [BasicPfx.Ast.PUSH(5); BasicPfx.Ast.PUSH(6); BasicPfx.Ast.REM];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #3 : Expected result 2*)
let cmds = [BasicPfx.Ast.PUSH(5); BasicPfx.Ast.PUSH(12); BasicPfx.Ast.DIV];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #4 : Expected result 60*)
let cmds = [BasicPfx.Ast.PUSH(5); BasicPfx.Ast.PUSH(12); BasicPfx.Ast.MUL];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;


(* Test #5 : Expected result  No result*)
let cmds = [];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #6 : Expected result  Error*)
let cmds = [BasicPfx.Ast.POP];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #7 : Expected result  Error*)
let cmds = [BasicPfx.Ast.MUL];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;

(* Test #8 : Expected result  Error*)
let cmds = [BasicPfx.Ast.REM];;
let state = (0,  cmds);;
let _ = BasicPfx.Eval.eval_program (state) [];;