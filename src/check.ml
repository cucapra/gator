(* Type checker *)

open Printf
open Ast

exception TypeException of string

(* AST pretty checker *)

open Ast

(* HashSet module, modified from prev projects *)
module HashSet = struct
  type ('a, 'b) t = (string, typ) Hashtbl.t
  let make() : ('a, 'b) t = Hashtbl.create 16
  let mem (h : ('a, 'b) t) (x : 'a) = Hashtbl.mem h x
  let add (h : ('a, 'b) t) (x : ('a* 'b)) =
    if mem h (fst x) then () else Hashtbl.add h (fst x) (snd x)
  let remove (h : ('a, 'b) t) (x : ('a* 'b)) =
    while Hashtbl.mem h (fst x) do
      Hashtbl.remove h (fst x)
    done
  let size (h : ('a, 'b) t) : int = Hashtbl.length h
  let values (h : ('a, 'b) t) : ('a* 'b) list =
    Hashtbl.fold (fun x y v -> (x, y) :: v) h []
  let find (h : ('a, 'b) t) (x: 'a) : 'b = try Hashtbl.find h x
    with Not_found -> raise (TypeException "Error - Cannot find var in gamma")
end

let gamma = HashSet.make ()

let check_vec (v: vec) : unit = ()

let check_mat (m: mat) : unit = ()
    
let rec check_ltyp (lt: ltyp) : unit =
    match lt with
    | VecTyp n -> if n < 0 then 
        (raise (TypeException "vec dimensions must be positive"))
    | MatTyp (n1, n2) -> if n1 < 0 || n2 < 0 then
        (raise (TypeException "mat dimensions must be positive"))
    | TagTyp s -> (let is_mem = HashSet.mem gamma s in 
        if not is_mem then (raise (TypeException "tag must be defined")
        ))
    | TransTyp (lt1, lt2) -> ()

let rec check_atyp (at: atyp) : typ = 
    match at with
    | IntTyp -> failwith "Unimplemented"
    | FloatTyp -> failwith "Unimplemented"
    | LTyp lt -> failwith "Unimplemented"

let rec check_btyp (bt: btyp) : typ = failwith "Unimplemented"

let rec check_typ (t: typ) : typ = 
    match t with
    | ATyp at -> check_atyp at
    | BTyp bt -> check_btyp bt

let rec check_aexp (a: aexp) : unit = 
    match a with
    | Const av -> ()
    | Var v -> ()
    | Lexp (a',l) -> failwith "Unimplemented"
    | Norm a -> check_aexp a
    | Dot (a1, a2)
    | Plus (a1, a2)
    | Times (a1, a2)
    | Minus (a1, a2)
    | CTimes (a1, a2) -> check_aexp a1; check_aexp a2

and check_bexp (b: bexp) : unit = 
    match b with 
    | True -> ()
    | False -> ()
    | Eq (a1, a2)
    | Leq (a1, a2) -> check_aexp a1; check_aexp a2
    | Or (b1, b2)
    | And (b1, b2) -> check_bexp b1; check_bexp b2
    | Not b' -> check_bexp b

let rec check_exp (e: exp) : unit = 
    match e with
    | Aexp a -> check_aexp a
    | Bexp b -> check_bexp b

let rec check_comm (c: comm) : unit =
    match c with
    | Skip -> ()
    | Print e -> check_exp e
    | Decl (t, s, e) -> failwith "Unimplemented"
    | If (b, c1, c2) -> failwith "Unimplemented"
    | _ -> failwith "Unimplemented" (* TODO - comment out *)
 

let rec check_comm_lst (cl : comm list) : unit = 
    match cl with
    | [] -> ()
    | h::t -> check_comm h; check_comm_lst t

let rec check_tags (t : tagdecl list) : unit =
    match t with 
    | [] -> ()
    | TagDecl(s, a)::t -> failwith "Unimplemented"

let check_prog (e : prog) : unit =
    match e with
    | Prog (t, c) -> check_tags t; check_comm_lst c
