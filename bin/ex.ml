open Langstart
open Ast

let _ =
    if (Array.length Sys.argv < 2) then 
    failwith "Given input file and optional arguments 'v' and 'p'" else
    let lexbuf = Lexing.from_channel (open_in (Array.get Sys.argv 1)) in
    let prog = Parser.main Lexer.read lexbuf in
    let _ = Check.check_prog prog in
    if (Array.length Sys.argv > 3) then print_endline (Print.print_prog prog) else ();
    print_string (Compiler.compile_program prog);
    if (Array.length Sys.argv > 2) then ((print_string "\n\n------------------\n\n");
        Ops.eval_prog prog) else ();
    print_string "\n"
