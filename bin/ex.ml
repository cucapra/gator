open Langstart
open Ast
open State

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.prog Lexer.token lexbuf in
    print_endline (Ops.pretty prog);
    print_string "= ";
    print_endline (Ops.eval_comm (snd prog) state.create).to_string
