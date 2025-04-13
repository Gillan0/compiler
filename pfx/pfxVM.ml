open BasicPfx

(* The arguments, initially empty *)
let args = ref []

(* The main function *)
let parse_eval file =
  print_string ("File " ^ file ^ " is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    begin
      try
        let pfx_prog = Parser.program Lexer.token lexbuf in
        Eval.eval_program pfx_prog !args
      with Parser.Error -> print_endline "Syntax error"
    end;
    close_in input_file
  with Sys_error _ -> print_endline ("Can't find file '" ^ file ^ "'")

(* Here we add the parsing of the command line and link to the main function *)
let _ =
  (* Function to register arguments *)
  let register_arg i = args := !args @ [i] in
  (* Process the command line arguments *)
  Arg.parse 
    [("-a", Arg.Int register_arg, "integer argument")]  (* List of options *)
    (fun _ -> ())  (* Function to handle anonymous arguments (none in this case) *)
    "Usage: program_name [options]";  (* Usage message *)

  (* Call parse_eval with a file path *)
  parse_eval "./pfx/tests/ok_prog.pfx";;
