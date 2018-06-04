(* A main read-eval-print loop for the parsing. *)

open Ast

(* Current program file and parsed program. *)
let file : string option ref = ref None
let program : comm option ref = ref None

let open_in (file : string) : in_channel =
  try open_in file
  with Sys_error s -> failwith ("Cannot open file: " ^ s)


(* Command handlers. *)
let load (filename : string) : unit =
  let ch = open_in filename in
  file := Some filename;
  let lexbuf = Lexing.from_channel ch in
  try
    let parse = Parser.main Lexer.read lexbuf in
    program := Some parse;
    close_in ch
  with
  | Parsing.Parse_error ->
    close_in ch;
    let pos = lexbuf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum in
    let tok = Lexing.lexeme lexbuf in
    failwith (Format.sprintf "Syntax error on line %d, column %d near %s" line col tok)
  (* | Error msg ->
    fprintf stderr "%s\n" lexbuf msg;
    close_in ch;
    failwith "Cannot parse program" *)

let _ =
  print_endline "parse tester";
  load Sys.argv.(0)
