(* Type checker *)
open Printf
open Ast
open Util

exception TypeException of string

let gamma = HashSet.make ()

let check_vec (v: vec) : typ = failwith "Unimplemented"

let check_mat (m: mat) : typ = failwith "Unimplemented"
    
let rec check_ltyp (lt: ltyp) : typ = 
    match lt with
    | VecTyp n -> if n < 0 then 
        (raise (TypeException "vec dimensions must be positive"));
        failwith "Unimplemented"
    | MatTyp (n1, n2) -> if n1 < 0 || n2 < 0 then
        (raise (TypeException "mat dimensions must be positive"));
        failwith "Unimplemented"
    | TagTyp s -> (let is_mem = HashSet.mem gamma s in 
        if not is_mem then (raise (TypeException "tag must be defined")
        ));failwith "Unimplemented"
    | TransTyp (lt1, lt2) -> failwith "Unimplemented"

let rec check_atyp (at: atyp) : typ = 
    match at with
    | IntTyp
    | FloatTyp -> ATyp at
    | LTyp lt -> check_ltyp lt

let rec check_typ (t: typ) : typ = 
    match t with
    | ATyp at -> check_atyp at
    | BTyp
    | UnitTyp -> t

let rec check_aval (av: avalue) : typ = 
    match av with
    | Num n -> failwith "Unimplemented"
    | Float f -> failwith "Unimplemented"
    | VecLit v -> failwith "Unimplemented"
    | MatLit m -> failwith "Unimplemented"

let rec check_exp (e: exp) : typ = 
    match e with
    | Bool b -> BTyp
    | Aval a -> check_aval a
    | Var v -> failwith "Unimplemented"
    | Lexp (a',l) -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Dot (e1, e2) -> failwith "Unimplemented"
    | Plus (e1, e2) -> failwith "Unimplemented"
    | Times (e1, e2) -> failwith "Unimplemented"
    | Minus (e1, e2) -> failwith "Unimplemented"
    | CTimes (e1, e2) -> failwith "Unimplemented"
    | Eq (e1, e2) -> failwith "Unimplemented"
    | Leq (e1, e2) -> failwith "Unimplemented"
    | Or (e1, e2) -> failwith "Unimplemented"
    | And (e2, e1) -> failwith "Unimplemented"
    | Not e1 -> check_exp e1

let rec check_comm (c: comm) : typ = 
    match c with
    | Skip -> UnitTyp
    | Print e -> check_exp e
    | Decl (t, s, e) -> failwith "Unimplemented"
    | If (b, c1, c2) -> failwith "Unimplemented"

let rec check_comm_lst (cl : comm list) : unit = 
    match cl with
    | [] -> failwith "Unimplemented"
    | h::t -> failwith "Unimplemented"

let rec check_tags (t : tagdecl list) : unit =
    match t with 
    | [] -> failwith "Unimplemented"
    | TagDecl(s, a)::t -> failwith "Unimplemented"

let check_prog (e : prog) : unit =
    match e with
    | Prog (t, c) -> check_tags t; check_comm_lst c