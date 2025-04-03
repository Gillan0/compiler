open FunPfx.Eval
open FunExpr.ToPfx
open FunExpr.Ast

(* f(x) = x + 2 Puis f(5)=7 *)
let expr = App(Fun("x", Binop(Badd, Var("x"), Const(2))) , Const(5));;

let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x) = x * ( x + 2 ) Puis f(3)=15 *)
let expr = App(Fun("x", Binop(Bmul, Var("x"), Binop(Badd, Var("x"), Const(2)))) , Const(3));;

let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* g(y) = y * 2 et f(x) = 5 + x Puis f(1) + g(2) = 10 *)
let expr = Binop(Badd, App(Fun("x", Binop(Badd, Const(5), Var("x"))) , Const(1)), App(Fun("y", Binop(Bmul, Const(2), Var("y"))) , Const(2)));;

let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* g(y) = y * 2 et f(x) = 5 + g(x) Puis f(3)=11 *)
let expr = App(Fun("x", Binop(Badd, Const(5), App(Fun("y", Binop(Bmul, Var("y"), Const(2))), Var("x")))), Const(3));;

let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

