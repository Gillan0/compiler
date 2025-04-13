open FunPfx.Eval
open FunPfx.Ast
open FunExpr.ToPfx
open FunExpr.Ast

let _ = Printf.printf "\n\n\n## Starting test for conversion from Expr to Pfx (vers. fun)\n"

(* f(x) = x + 1 puis f(2)=3 *)
let expr = App(Fun("x", Binop(Badd, Var("x"), Const(1))), Const(2));;
let pfx_cmds = generate expr;;
Printf.printf "Commandes Q°10.1 : \n%s\n" (string_of_commands pfx_cmds);;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x) = x + 2 puis f(5)=7 *)
let expr = App(Fun("x", Binop(Badd, Var("x"), Const(2))), Const(5));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x) = 2 + x puis f(5)=7 *)
let expr = App(Fun("x", Binop(Badd, Const(2), Var("x"))) , Const(5));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x) = x * ( x + 2 ) puis f(3)=15 *)
let expr = App(Fun("x", Binop(Bmul, Var("x"), Binop(Badd, Var("x"), Const(2)))) , Const(3));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x) = ( x + 2 ) * x puis f(3)=15 *)
let expr = App(Fun("x", Binop(Bmul, Binop(Badd, Var("x"), Const(2)), Var("x") )) , Const(3));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* g(y) = y * 2 et f(x) = 5 + x puis f(1) + g(2) = 10 *)
let expr = Binop(Badd, App(Fun("x", Binop(Badd, Const(5), Var("x"))) , Const(1)), App(Fun("y", Binop(Bmul, Const(2), Var("y"))) , Const(2)));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* g(y) = y * 2 et f(x) = 5 + g(x) puis f(3)=11 *)
let expr = App(Fun("x", Binop(Badd, Const(5), App(Fun("y", Binop(Bmul, Var("y"), Const(2))), Var("x")))), Const(3));;
let pfx_cmds = generate expr;;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* h(x, y) = x - y puis h(12, 8) = 4 *)
let expr = App(Fun("x", App(Fun("y", Binop(Bsub, Var("x"), Var("y"))), Const(8))), Const(12));;
let pfx_cmds = generate expr;;
Printf.printf "Commandes Q°10.4 : \n%s\n" (string_of_commands pfx_cmds);;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* f(x, y, z) = x - y * z puis h(16, 2, 3) = 10 *)
let expr = App(Fun("x", 
              App(Fun("y", 
                  App(Fun("z", 
                      Binop(Bsub, 
                          Var("x"), 
                          Binop(Bmul, 
                                Var("y"), 
                                Var("z")
                          )
                      )), Const(3)
                    )),  Const(2)
                  )), Const(16));;
let pfx_cmds = generate expr;;
Printf.printf "Command test 3 var: \n%s\n" (string_of_commands pfx_cmds);;
let state = (0,  pfx_cmds);;
let _ = eval_program (state) [];;

(* Error testing *)

(* Unbound variable. Should return error *)
let expr = Binop(Bsub, Var("x"), Const(12));;
try 
  let _ = generate expr in ()
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

(* Unbound variable. Should return error*)
let expr = App(Fun("x", Binop(Bsub, Var("x"), Var("y"))), Const(12));;
try 
  let _ = generate expr in ()
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

(* 2 nested functions with same variable name. Should return error *)
let expr = App(Fun("x", App(Fun("x", Binop(Bsub, Var("x"), Var("x"))), Const(8))), Const(12));;
try 
  let _ = generate expr in ()
with
| Failure msg -> Printf.printf "Error: %s\n" msg
| e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)


let _ = Printf.printf "## Ending test for conversion from Expr to Pfx (vers. fun)\n\n\n"