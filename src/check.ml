(* Type checker *)
open Printf
open Ast
open Util
open Print
open Str
exception TypeException of string

(* Variable defs *)
let gamma = HashSet.make ()

(* Tags defs *)
let delta = HashSet.make ()


(* Checks equality of the dimensions of ltyp *)
let rec ltyp_dim_equals (t1: ltyp) (t2: ltyp) : bool =
    match (t1, t2) with 
    | (VecTyp n1, VecTyp n2) -> n1 = n2
    | (VecTyp n1, MatTyp (n2, n3)) -> n1 = n2
    | (MatTyp (n1, n2), VecTyp n3) -> n2 = n3
    | (MatTyp (n1, n2), MatTyp (n3, n4)) -> n1 = n3 && n2 = n4
    | (TagTyp i1, TagTyp i2) -> 
        ltyp_dim_equals (HashSet.find delta i1) (HashSet.find delta i2)
    | (TransTyp (lt1, lt2), TransTyp (lt3, lt4)) -> 
        ltyp_dim_equals lt1 lt3 && ltyp_dim_equals lt2 lt4
    | _ -> false

(* Gets dimension for top type of ltyp *)
let rec ltyp_top_dim (t: ltyp) : int * int = 
    match t with
    | VecTyp n -> (1, n)
    | MatTyp (n1, n2) -> (n1, n2)
    | TagTyp a -> HashSet.find delta a |> ltyp_top_dim
    | TransTyp (lt1, lt2) -> (ltyp_top_dim lt1 |> fst, 
        ltyp_top_dim lt2 |> snd)  (* TODO - need to consider vec/mat *)

(* Gets top type of ltyp *)
let ltyp_top_typ (t: ltyp) : ltyp = 
    match t with 
    | TagTyp _ 
    | TransTyp _ -> let dim = ltyp_top_dim t in 
        MatTyp (fst dim, snd dim)
    | _ -> t

(* let's ignore subtyping for now *)
(* Infix subtype operator for types *)
(* Following <Section 2. Subtype Ordering> of semantics *)
(* TODO - tag type subtyping is janky (not a string match) *)
(* let rec (<~) (t1: ltyp) (t2: ltyp) : bool = 
    match (t1, t2) with 
    | (VecTyp n1, MatTyp(1, n2))
    | (MatTyp(1, n1), VecTyp n2) -> n1 = n2
    | (TagTyp i1, TagTyp i2) -> ltyp_equals (HashSet.find delta i1) (HashSet.find delta i2) 
    | (l1, l2) -> (ltyp_equals l1 l2) || 
        (ltyp_equals (ltyp_top_typ l1) l2)  *)

(* Checks ltyp equality *)
(* and ltyp_equals (t1: ltyp) (t2: ltyp) : bool = t1 <~ t2 && t2 <~ t1 *)
   
(* Checks dimensions of ltyp for transformations *)
(* Returns true if dimensions are valid *)
let rec ltyp_dim_trans (t1: ltyp) (t2: ltyp) : bool =
    match (t1, t2) with 
    | (VecTyp n1, VecTyp n2) -> n1 = n2
    | (MatTyp (n1, n2), MatTyp (n3, n4)) -> n2 == n3
    | (TagTyp i1, TagTyp i2) -> 
        ltyp_dim_trans (HashSet.find delta i1) (HashSet.find delta i2)
    | (TransTyp (lt1, lt2), TransTyp (lt3, lt4)) -> ltyp_dim_equals lt2 lt3
    | _ -> false

(* Type check linear types *)
let rec check_ltyp (lt: ltyp) : typ = 
    match lt with
    | VecTyp n -> if n < 0 
        then (raise (TypeException "vec dimensions must be positive"))
        else ATyp(LTyp(lt))
    | MatTyp (n1, n2) -> if n1 < 0 || n2 < 0 then
        (raise (TypeException "mat dimensions must be positive"))
        else ATyp(LTyp(lt))
    | TagTyp s -> let is_mem = HashSet.mem delta s in 
        if not is_mem then (
            (raise (TypeException ("tag "^s^" must be defined")))
        ) else ATyp(LTyp(lt))
    | TransTyp (lt1, lt2) -> if ltyp_dim_trans lt1 lt2 |> not
        then (raise (TypeException "transformation dimension mismatch"))
        else  ATyp(LTyp(lt))

(* Type check arithmetic types *)
let rec check_atyp (at: atyp) : typ = 
    match at with
    | IntTyp
    | FloatTyp -> ATyp at
    | LTyp lt -> check_ltyp lt

(* Type check types *)
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
    | VecLit (v, t) -> 
        (* let littyp = veclit_type v in  *)
        (* if littyp <~ t *)
        (* then  *)
        ATyp(LTyp(t)) 
        (* else (raise (TypeException "vec literal tag mismatch")) *)
    | MatLit (m, t) -> 
        (* let littyp = matlit_type m in 
        if littyp <~ t
        then  *)
        ATyp(LTyp(t)) 
        (* else (raise (TypeException "mat literal tag mismatch")) *)

(* Type checking binary operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_scalar_binop (t1: typ) (t2: typ) : typ =
    match (t1, t2) with 
    | (ATyp(IntTyp), ATyp(a))
    | (ATyp(a), ATyp(IntTyp)) 
    | (ATyp(FloatTyp), ATyp(a)) 
    | (ATyp(a), ATyp(FloatTyp)) -> ATyp a
    | (ATyp(LTyp a1), ATyp(LTyp a2)) -> if ltyp_dim_equals a1 a2 then t1 
        else (raise (TypeException ("dimension mismatch for arithmetic operation")))
    | _ -> 
        (raise (TypeException ("invalid expressions for arithmetic operation: "^(print_typ t1)^", "^(print_typ t2))))

(* "scalar linear exp", (i.e. dot and ctimes) returns generalized MatTyp *)
let check_scalar_linear_exp (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with 
    | (ATyp(LTyp l1), ATyp(LTyp l2)) -> if ltyp_dim_equals l1 l2 
        then ATyp(LTyp(ltyp_top_typ l1))
        else (raise (TypeException "dimension mismatch in dot operator"))
    | _ -> (raise (TypeException "expected linear types for dot operator"))

(* Type check norm expressions *)
let check_norm_exp (a: typ) : typ = 
    match a with
    | ATyp(LTyp l) -> ATyp(LTyp(ltyp_top_typ l))
    | _ -> (raise (TypeException "expected linear type for norm operator"))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with 
    | (BTyp, BTyp) -> BTyp
    | _ -> raise (TypeException "expected boolean expression for binop")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t1: typ) : typ =
    match t1 with 
    | BTyp -> BTyp
    | _ -> raise (TypeException "expected boolean expression for !")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only bool, int, float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with
    | (BTyp, BTyp) -> BTyp
    | (ATyp(IntTyp), ATyp(IntTyp)) -> BTyp
    | (ATyp(FloatTyp), ATyp(FloatTyp)) -> BTyp
    | _ -> raise (TypeException "unexpected type for binary comparator operations")

let check_dot_exp (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with 
    | (ATyp(LTyp _ ), ATyp(LTyp _)) -> ATyp(IntTyp)
    | _ -> raise (TypeException "unexpected type for dot product exp")

(* Type checking times operator - on scalar mult & matrix transformations *)
let rec check_times_exp (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with
    | (ATyp(LTyp(MatTyp(n1, n2))), ATyp(LTyp(MatTyp(n3, n4)))) -> 
        if n2 = n3 
        then ATyp(LTyp(MatTyp(n1, n4))) 
        else (raise (TypeException "matrix multiplication dimension mismatch"))
    | (ATyp(LTyp(TransTyp(lt1, lt2))), ATyp(LTyp(TransTyp(lt3, lt4)))) ->
        (* if ltyp_equals lt2 lt3
        then  *)
        ATyp(LTyp(TransTyp(lt1, lt4)))
        (* else (raise (TypeException "linear transformation type mismatch")) *)
    | (ATyp(LTyp a), ATyp(LTyp(TransTyp(lt1, lt2)))) ->
        (* if ltyp_equals a lt1 
        then  *)
        ATyp(LTyp(lt2))
        (* else (raise (TypeException "linear transformation type mismatch")) *)
    | _ -> check_scalar_binop t1 t2

and check_exp (e: exp) : typ = 
    match e with
    | Bool b -> BTyp
    | Aval a -> check_aval a
    | Var v -> HashSet.find gamma v
    | Norm a -> check_norm_exp (check_exp a)
    | Dot (e1, e2) -> check_dot_exp (check_exp e1) (check_exp e2)
    | Plus (e1, e2)
    | Minus (e1, e2) -> check_scalar_binop (check_exp e1) (check_exp e2)
    | Times (e1, e2) -> check_times_exp (check_exp e1) (check_exp e2)
    | CTimes (e1, e2) -> check_scalar_linear_exp (check_exp e1) (check_exp e2)
    | Eq (e1, e2)
    | Leq (e1, e2) -> check_comp_binop (check_exp e1) (check_exp e2)
    | Or (e1, e2)
    | And (e1, e2) -> check_bool_binop (check_exp e1) (check_exp e2)  
    | Not e1 -> check_bool_unop (check_exp e1)
    | Typ typ -> check_typ typ

let rec check_decl (t: typ) (s: string) (e: exp) : typ =
    let etyp = check_exp e in
    let t' = check_typ t in
    match (etyp, t') with
    | (ATyp(LTyp a1), ATyp(LTyp a2)) -> 
        (* if a1 <~ a2 
        then  *)
        (HashSet.add gamma (s, t'); UnitTyp)
        (* else raise (TypeException "mismatched linear type for var decl") *)
    | (ATyp(IntTyp), ATyp(IntTyp))
    | (ATyp(FloatTyp), ATyp(FloatTyp))
    | (BTyp, BTyp) -> (HashSet.add gamma (s, t'); UnitTyp)
    | _ -> raise (TypeException "mismatched types for var decl")

let rec check_comm (c: comm) : typ = 
    match c with
    | Skip -> UnitTyp
    | Print e -> ignore(check_exp e); UnitTyp
    | Decl (t, s, e) -> check_decl t s e
    | If (b, c1, c2) -> check_comm_lst c1; check_comm_lst c2; 
        (match check_exp b with 
        | BTyp -> UnitTyp
        | _ -> raise (TypeException "expected boolean expression for if condition"))

and check_comm_lst (cl : comm list) : unit = 
    match cl with
    | [] -> ()
    | h::t -> ignore(check_comm h); check_comm_lst t

(* TODO - maybe check vars for duplicates with tags *)
let rec check_tags (t : tagdecl list) : unit =
    match t with 
    | [] -> ()
    | TagDecl(s, a)::t -> 
        ignore(check_atyp a);
        match a with 
        | (LTyp l) -> HashSet.add delta (s, l); check_tags t
        | _ -> raise (TypeException "expected linear type for tag declaration")

let check_prog (e : prog) : unit =
    match e with
    | Prog (t, c) -> check_tags t; check_comm_lst c