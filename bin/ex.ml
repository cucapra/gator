open Langstart
open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.main Lexer.read lexbuf in
    if (Array.length Sys.argv > 1) then print_endline (Print.print_prog prog) else ();
    Ops.eval_prog prog;
    print_string "\n"
