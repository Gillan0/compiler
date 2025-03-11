(* TO BE EVALUATED IN UTOP *)

let stack = [];;
let cmds = [PUSH(1); PUSH(5); SWAP; POP; PUSH(8); ADD];;

let state = (cmds,stack);;