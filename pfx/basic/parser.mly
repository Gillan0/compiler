%{
  (* Ocaml code here*)

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)

%token EOF
%token ADD
%token SUB
%token MUL
%token DIV
%token REM
%token POP
%token SWAP
%token <int> INT
%token <int> PUSH

(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)

program: 
  |i=INT EOF { i, []}
  |i=INT instructions EOF { i, [] }

instructions :
  | instr {[$1]}
  | instr instructions {$1 :: $2}

instr:
  | SWAP { Ast.SWAP }
  | PUSH i=INT { Ast.PUSH(i) }
  | POP { Ast.POP }
  | ADD { Ast.ADD }
  | SUB { Ast.SUB }
  | MUL { Ast.MUL }
  | DIV { Ast.DIV }
  | REM { Ast.REM }
  

%%
