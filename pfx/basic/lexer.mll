{
  (*
  open Parser
  *)
  type token =
  |PUSH of int
  |POP
  |SWAP
  |ADD
  |SUB
  |MUL
  |DIV
  |REM 
  |INT of int
  |EOF;;

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
  | EOF -> print_string "EOF";;

  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  |"add" { ADD }
  |"sub" { SUB }
  |"mul" { MUL }
  |"div" { DIV }
  |"rem" { REM }
  |"pop" { POP }
  |"swap" { SWAP }
  |"push" digit+ as nb {PUSH(int_of_string nb)}
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }

{
  let rec examine_all lexbuf =
  let result = token lexbuf in
  print_token result;
  print_string " ";
  match result with
  | EOF -> ()
  | _ -> examine_all lexbuf

let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in input_file
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

  let _ = Arg.parse [] compile "./tests/ok_prog.pfx"

}