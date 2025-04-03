{
  open Parser

  open Utils

  let print_token = function
  | PUSH i -> Printf.printf "PUSH %d" i
  | POP -> print_string "POP"
  | SWAP -> print_string "SWAP"
  | ADD -> print_string "ADD"
  | SUB -> print_string "SUB"
  | MUL -> print_string "MUL"
  | DIV -> print_string "DIV"
  | REM -> print_string "REM"
  | INT i -> Printf.printf "INT %d" i
  | GET -> print_string "GET"
  | EXEC_SEQ -> print_string "EXEC_SEQ"
  | LPAR -> print_string "("
  | RPAR -> print_string ")"
  | EXEC -> print_string "EXEC"
  | EOF -> print_string "EOF";;

  let mk_int nb loc=
    try INT (int_of_string nb)
    with Failure _ -> raise (Location.Error(Printf.sprintf "Illegal integer '%s': " nb, loc))
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { Location.incr_line lexbuf; token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb (Location.curr lexbuf)}
  (* commands  *)
  |"add" { ADD }
  |"sub" { SUB }
  |"mul" { MUL }
  |"div" { DIV }
  |"rem" { REM }
  |"pop" { POP }
  |"swap" { SWAP }
  |"push" blank* (digit+ as nb) {PUSH(int_of_string nb)}
  |"get" {GET}
  |"exec" { EXEC }  
  |"exec_seq" { EXEC_SEQ }  
  |"(" { LPAR }  
  |")" { RPAR }  
  (* illegal characters *)
  | _ as c                  { raise (Location.Error(Printf.sprintf "Illegal character '%c': " c, Location.curr lexbuf)) }
