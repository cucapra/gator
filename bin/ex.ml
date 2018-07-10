open Lingl
open Ast

let program : Ast.prog option ref = ref None


let _ =
    if (Array.length Sys.argv < 2) then 
    failwith "Given input file and optional arguments 'v' and 'p'" else
    let ch =
        try open_in (Array.get Sys.argv 1)
        with Sys_error s -> failwith ("Cannot open file: " ^ s) in
    let prog : Ast.prog =
        let lexbuf = Lexing.from_channel ch in
        try
            Parser.main Lexer.read lexbuf
        with _ ->
            begin
                close_in ch;
            failwith ("Parsing error at character " ^ string_of_int (Lexing.lexeme_end lexbuf))
            end in  
        close_in ch;
    let _ = Check.check_prog prog in
    if (Array.length Sys.argv > 3) then print_endline (Print.print_prog prog);
    print_string (Compiler.compile_program prog);
    if (Array.length Sys.argv > 2) then ((print_string "\n\n------------------\n\n");
        Ops.eval_prog prog);
    print_string "\n"
