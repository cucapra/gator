(* Main driver for parsing, for unit testing*)

open CoreAst
open GatorAst
open GatorAstPrinter

(* Current program file and parsed program. *)
let file : string option ref = ref None
let program : prog option ref = ref None

let open_in (file : string) : in_channel =
  try open_in file
  with | Sys_error s -> failwith ("Cannot open file: " ^ s)
  | _ -> failwith ("Cannot open file")

(* Load, lex & parse a file *)
let load (filename : string) : unit =
  let ch = open_in filename in
  file := Some filename;
  let lexbuf = Lexing.from_channel ch in
  try
    let parse = Parser.main Lexer.read lexbuf in
    let prog = parse in
    let prog_str = string_of_prog prog in
    print_endline prog_str;
    close_in ch
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at %d:%d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  | _ ->
    close_in ch;
    let pos = lexbuf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum in
    let tok = Lexing.lexeme lexbuf in
    failwith (Format.sprintf "Syntax error on line %d, column %d near %s" line col tok)
    

let _ = 
  print_endline "Parser tester";
  load Sys.argv.(1)
