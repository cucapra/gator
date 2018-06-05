(* Type checker *)

open Printf
open Ast

exception TypeCheckException of string

let check_vec (v: vec) : unit = 
    failwith "Unimplemented"

let check_mat (m: mat) : unit = 
    failwith "Unimplemented"

let rec check_ltyp (lt: ltyp) : unit =
    failwith "Unimplemented"

let rec check_atyp (at: atyp) : unit = 
    failwith "Unimplemented"

let rec check_btyp (bt: btyp) : unit = 
    failwith "Unimplemented"

let rec check_typ (t: typ) : unit = 
    failwith "Unimplemented"

let rec check_aval (av: avalue) : unit = 
    failwith "Unimplemented"
  
let rec check_aexp (a: aexp) : unit = 
    failwith "Unimplemented"

let rec check_bexp (b: bexp) : unit = 
    failwith "Unimplemented"

let rec check_exp (e: exp) : unit = 
    failwith "Unimplemented"

let rec check_comm (c: comm) : unit =
    failwith "Unimplemented"

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
    | Prog (t, c) -> failwith "Unimplemented"
