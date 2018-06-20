open Ast
open Context
open Lin_ops
open Util

type delta = (ltyp, ltyp) Context.context

let rec comp_exp (e : exp) (d : delta) : string =
    match e with
    | Aval a -> (match a with 
        | Num n -> string_of_int n
        | Float f -> string_of_float f
        | VecLit (v, _) -> string_of_vec v
        | MatLit (m, _) -> string_of_mat m)

    | Var x -> x
    | Dot (a1, a2) -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Plus (a1, a2) -> failwith "Unimplemented"
    | Times (a1, a2) -> failwith "Unimplemented"
    | Minus (a1, a2) -> failwith "Unimplemented"
    | CTimes (a1, a2) -> failwith "Unimplemented"
    | Bool b -> string_of_bool b 
    | Eq (a1, a2) -> failwith "Unimplemented"
    | Leq (a1, a2) -> failwith "Unimplemented"
    | Or (b1, b2) -> failwith "Unimplemented"
    | And (b1, b2) -> failwith "Unimplemented"
    | Not b -> failwith "Unimplemented"

let rec comp_comm (c : comm list) (d : delta) : string =
    match c with
    | [] -> ""
    | h::t -> comp_comm t (match h with
        | Skip -> d
        | Print e -> failwith "Unimplemented"
        | Decl (t, x, e) -> failwith "Unimplemented"
        | If (e, c1, c2) -> failwith "Unimplemented")

let rec compile_program (p : prog) : string =
    match p with
    | Prog (_, c) -> comp_comm c Context.empty