%{
  (* Ocaml code here*)
  open Ast
%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF POP SWAP ADD SUB MUL DIV REM
%token <int> INT
%token <int> PUSH

(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program
%type <Ast.command list> instructions
%type <Ast.command> command

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)

program:
| i=INT  EOF { i, [] }
| i=INT instr=instructions EOF { i, instr }

instructions:
| c=command e=instructions {c :: e}
| c=command {[c]}

command:
| i=PUSH {PUSH(i)}
| POP { POP }
| SWAP { SWAP }
| ADD { ADD }
| SUB { SUB }
| MUL { MUL }
| DIV { DIV }
| REM { REM }

%%
