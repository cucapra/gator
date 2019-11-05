open Gatorl

let program : GatorAst.prog option ref = ref None

let program_file : string option ref = ref None
let set_program_file (arg : string) : unit = 
    match !program_file with
    | None -> program_file := Some arg
    | Some _ -> ()  (* Don't overwrite program_file *)

let run_interp : bool ref = ref false
let emit_ts : bool ref = ref false
let debug_flag : bool ref = ref false

let usage_msg = "Gator Help Center\n"
let spec_list : (Arg.key * Arg.spec * Arg.doc) list =
    [
        ("-i", Arg.Set run_interp,
        "Runs the given file with the gator interpreter (replaces standard output)");
        ("-t", Arg.Set emit_ts,
        "Emits Typescript (replaces standard output)");
        ("-d", Arg.Set debug_flag,
        "Enable debug output")
    ]

let prog_path f = if not (String.contains f '/') then "" else
    String.concat "/" (List.rev (List.tl (List.rev (String.split_on_char '/' f)))) ^ "/"

let parse_prog f : GatorAst.prog =
    let ch =
        try open_in f
        with Sys_error s -> failwith ("Cannot open file: " ^ s) in
    let prog : GatorAst.prog =
        let lexbuf = Lexing.from_channel ch in
        try
            Parser.main Lexer.read lexbuf
        with 
            | _ ->
                begin
                    close_in ch;
                let pos = lexbuf.Lexing.lex_curr_p in
                let tok = (Lexing.lexeme lexbuf) in
                (* let line = pos.Lexing.pos_lnum in *)
                let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
                failwith ("Parsing error at token '" ^ tok ^ "', line "
                     ^ (string_of_int pos.Lexing.pos_lnum) ^ ", column " ^ string_of_int cnum)
                end in
    close_in ch; prog

let rec search_progs path fs found : GatorAst.prog Assoc.context = 
    match fs with
    | [] -> Assoc.empty
    | f::t -> 
        let p = parse_prog (path ^ f) in
        let to_search,found' = Check.search_prog p found in
        Assoc.update f p (search_progs (path ^ (prog_path f)) to_search found')

let _ =
    Arg.parse spec_list set_program_file usage_msg;
    Util.debug := !debug_flag;
    match !program_file with
    None -> print_string (Arg.usage_string spec_list usage_msg) | Some f ->
    let prog = parse_prog f in
    let progname = List.hd (String.split_on_char '.'  (List.hd (List.rev (String.split_on_char '/' f)))) in
    let fs,found = Check.search_prog prog [progname] in
    let typedProg, params = Check.check_prog prog (search_progs (prog_path f) fs found) in
    if !run_interp then Ops.eval_prog typedProg params
    else if !emit_ts then print_string (EmitTS.compile_program typedProg params)
    else print_string (EmitGL.compile_program typedProg params)
