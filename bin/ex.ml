open Langstart
open Ast
open State

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.prog Lexer.token lexbuf in
    print_endline (Ops.pretty expr);
    print_string "= ";
    print_endline (Ops.eval_comm expr state.create).to_string
