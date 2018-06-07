(* Type checker *)
open Printf
open Ast
open Util

exception TypeException of string

let gamma = HashSet.make ()

(* Checks ltyp equality *)
let ltyp_equals (t1: ltyp) (t2: ltyp) : bool = failwith "Unimplemented"    

(* Checks equality of the dimensions of ltyp *)
let rec ltyp_dim_equals (t1: ltyp) (t2: ltyp) : bool =
    match (t1, t2) with 
    | (VecTyp n1, VecTyp n2) -> n1 = n2
    | (MatTyp (n1, n2), MatTyp (n3, n4)) -> n1 = n3 && n2 = n4
    | (TagTyp i1, TagTyp i2) -> failwith "Unimplemented"
    | (TransTyp (lt1, lt2), TransTyp (lt3, lt4)) -> 
        ltyp_dim_equals lt1 lt3 && ltyp_dim_equals lt2 lt4
    | _ -> false

let rec check_ltyp (lt: ltyp) : typ = 
    match lt with
    | VecTyp n -> if n < 0 then 
        (raise (TypeException "vec dimensions must be positive"));
        ATyp(LTyp(lt))
    | MatTyp (n1, n2) -> if n1 < 0 || n2 < 0 then
        (raise (TypeException "mat dimensions must be positive"));
         ATyp(LTyp(lt))
    | TagTyp s -> (let is_mem = HashSet.mem gamma s in 
        if not is_mem then (raise (TypeException "tag must be defined")
        )); ATyp(LTyp(lt))
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

(* Gets type of vector literals *)
let veclit_type (v: vec) : ltyp = VecTyp (List.length v)

(* Helper function for matrix literals
   Every vec in a mat needs to be of the same dimension  *)
let rec matlit_type_helper (m: mat) (dim: int) : bool =
    match m with
    | [] -> true
    | h::t -> dim = List.length h && matlit_type_helper t dim 
   
(* Gets type of matrix literals *)
let matlit_type (m: mat) : ltyp =  
    match m with 
    | [] -> MatTyp(0,0)
    | _ -> let dim = (List.hd m |> List.length) in
        if matlit_type_helper m dim
        then MatTyp(List.length m, dim)
        else (raise (TypeException "mat dimension inconsistent"))

(* Type checking arithmetic literals *)
let rec check_aval (av: avalue) : typ = 
    match av with
    | Num n -> ATyp(IntTyp)
    | Float f -> ATyp(FloatTyp)
    | VecLit (v, t) -> let littyp = veclit_type v in 
        if ltyp_equals littyp t
        then ATyp(LTyp(t)) 
        else (raise (TypeException "vec literal tag mismatch"))
    | MatLit (m, t) -> let littyp = matlit_type m in 
        if ltyp_equals littyp t
        then ATyp(LTyp(t)) 
        else (raise (TypeException "mat literal tag mismatch"))

(* Type checking binary operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_scalar_binop (t1: typ) (t2: typ) : typ =
    match (t1, t2) with 
    | (ATyp(IntTyp), ATyp(a))
    | (ATyp(a), ATyp(IntTyp)) 
    | (ATyp(FloatTyp), ATyp(a)) 
    | (ATyp(a), ATyp(FloatTyp)) -> ATyp a
    | _ -> (raise (TypeException "invalid expressions for arithmetic operation"))

(* Type cheking times operator - on scalar mult & matrix transformations *)
let rec check_times_exp (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with
    | (ATyp(LTyp(MatTyp(n1, n2))), ATyp(LTyp(MatTyp(n3, n4)))) -> 
        if n2 = n3 
        then ATyp(LTyp(MatTyp(n1, n4))) 
        else (raise (TypeException "matrix multiplication dimension mismatch"))
    | (ATyp(LTyp(TransTyp(lt1, lt2))), ATyp(LTyp(TransTyp(lt3, lt4)))) ->
        if ltyp_equals lt2 lt3
        then ATyp(LTyp(TransTyp(lt1, lt4)))
        else (raise (TypeException "linear transformation type mismatch"))
    | (ATyp(LTyp a), ATyp(LTyp(TransTyp(lt1, lt2)))) ->
        if ltyp_equals a lt1 
        then ATyp(LTyp(lt2))
        else (raise (TypeException "linear transformation type mismatch"))
    | _ -> check_scalar_binop t1 t2

and check_exp (e: exp) : typ = 
    match e with
    | Bool b -> BTyp
    | Aval a -> check_aval a
    | Var v -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Dot (e1, e2) -> failwith "Unimplemented"
    | Plus (e1, e2)
    | Minus (e1, e2) -> check_scalar_binop (check_exp e1) (check_exp e2)
    | Times (e1, e2) -> check_times_exp (check_exp e1) (check_exp e2)
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