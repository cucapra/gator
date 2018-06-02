open Langstart
open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.prog Lexer.token lexbuf in
    print_endline (Ops.pretty expr);
    print_string "= ";
    print_endline (match fst (Ops.eval expr []) with
            | Int x -> string_of_int x
            | Bool x -> string_of_bool x
            | Null -> "None")
