open BasicPfx.Lexer

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
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ = Printf.printf "\n\n\n## Starting test for compilation in basic Pfx\n"

let _ = Printf.printf "\n# First test : Expected result Error at line 4 \n";;
try 
  let _ = compile "./error_prog.pfx" in ()
with
| e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

let _ = Printf.printf "\n# Second test : Expected result INT 0 PUSH 2 PUSH 7 PUSH 3 ADD DIV EOF \n";;
try 
  let _ = compile "./ok_prog.pfx" in ()
with
| e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)


let _ = Printf.printf "\n## Ending test for compilation in basic Pfx\n\n\n"