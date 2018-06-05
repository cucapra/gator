open Langstart
open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.main Lexer.token lexbuf in
    print_endline (Print.pretty prog);
    print_string "= ";
    print_endline (Print.eval_comm (snd prog) State.create).to_string
