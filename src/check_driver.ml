(* Main driver for type checking, for unit testing*)

open Ast
open Print

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
    let parse = Parser.main Lexer.read lexbuf in
    let prog = parse in
    let prog_str = print_prog prog in
    Check.check_prog prog;
    print_endline prog_str;
    close_in ch
  
let _ = 
  print_endline "Parser tester";
  load Sys.argv.(1)