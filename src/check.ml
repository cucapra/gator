open Ast
open Context
open Util
open Print
open Printf
open Str

exception TypeException of string

(* Variable defs *)
type gamma = (string, typ) Context.context

(* Tags defs *)
type delta = (string, ltyp) Context.context

type ltyp_top = 
    | VecDim of int
    | MatDim of int * int 

(* Gets dimension for top type of ltyp *)
let rec ltyp_top_dim (t: ltyp) (d : delta) : int * int = 
    debug_print ">> ltyp_top_dim";
    match t with
    | VecTyp n -> (1, n)
    | MatTyp (n1, n2) -> (n1, n2)
    | TagTyp a -> 
        (* "\tTagTyp - [ "^a^", top dim: ("^(ltyp_top_dim (Context.lookup d t) d |> snd |> string_of_int)^
            ", "^(HashSet.find delta a |> ltyp_top_dim |> snd |> string_of_int)^") ]" |> debug_print;  *)
        ltyp_top_dim (Context.lookup d a) d
    | TransTyp (lt1, lt2) -> 
    "\tTranstyp - [ "^(ltyp_top_dim lt1 d |> snd |> string_of_int)^","^(ltyp_top_dim lt2 d |> fst |> string_of_int)^" ]" |> debug_print; 
        (ltyp_top_dim lt2 d |> snd, ltyp_top_dim lt1 d |> snd) 

(* Checks equality of the dimensions of ltyp *)
let rec ltyp_dim_equals (t1: ltyp) (t2: ltyp) (d: delta) : bool =
    debug_print ">> ltyp_dim_equals";
    match (t1, t2) with 
    | (VecTyp n1, VecTyp n2) -> debug_print "\tvec, vec"; 
        n1 = n2
    | (VecTyp n1, MatTyp (n2, n3)) -> debug_print "\tvec, mat";
        n2 = 1 && n1 = n3
    | (MatTyp (n1, n2), VecTyp n3) -> debug_print "\tmat, vec";
        n2 = n3
    | (MatTyp (n1, n2), MatTyp (n3, n4)) -> debug_print "\tmat, mat";
        n1 = n3 && n2 = n4
    | (TagTyp i, VecTyp _)
    | (TagTyp i, MatTyp _) -> 
        debug_print ("\t"^(ATyp(LTyp(t2)) |> print_typ));  
        ltyp_dim_equals (Context.lookup d i) t2 d
    | (VecTyp _, TagTyp i)
    | (MatTyp _, TagTyp i) -> ltyp_dim_equals (Context.lookup d i) t1 d
    | (TagTyp i1, TagTyp i2) -> 
        ltyp_dim_equals (Context.lookup d i1) (Context.lookup d i2) d
    | (MatTyp (n1, n2), TransTyp (lt1, lt2))
    | (TransTyp (lt1, lt2), MatTyp (n1, n2)) -> let top_dim = ltyp_top_dim (TransTyp (lt1, lt2)) d in 
        "\tTranstyp"^(string_of_int(n2)) |> debug_print; 
        top_dim |> fst = n1 && top_dim |> snd = n2
    | (TransTyp (lt1, lt2), TransTyp (lt3, lt4)) -> 
        ltyp_dim_equals lt1 lt3 d && ltyp_dim_equals lt2 lt4 d
    | _ -> false

(* Gets top type of ltyp *)
let ltyp_top_typ (t: ltyp) (d: delta) : ltyp = 
    debug_print ">> ltyp_top_typ";
    match t with 
    | TagTyp _ 
    | TransTyp _ -> let dim = ltyp_top_dim t d in 
        MatTyp (fst dim, snd dim)
    | _ -> t

(* Infix subtype operator for types *)
(* Following <Section 2. Subtype Ordering> of semantics *)
let rec is_subtype (t1: ltyp) (t2: ltyp) (d: delta) : bool = 
    debug_print ">> (<~)";
    match (t1, t2) with 
    | (VecTyp n1, MatTyp(1, n2))
    | (MatTyp(1, n1), VecTyp n2) -> n1 = n2
    | (MatTyp _, MatTyp _)
    | (VecTyp _, TagTyp _) 
    | (MatTyp _, TagTyp _) 
    | (TagTyp _, MatTyp _)
    | (TransTyp _, MatTyp _)
    | (MatTyp _, TransTyp _)
    | (TagTyp _, VecTyp _) -> ltyp_dim_equals t1 t2 d
    | (TagTyp i1, TagTyp i2) -> is_subtype (Context.lookup d i1) (Context.lookup d i2) d
    | (l1, l2) -> (ltyp_dim_equals l1 l2 d) || 
        (ltyp_dim_equals (ltyp_top_typ l1 d) l2 d) 

(* Checks dimensions of ltyp for transformations *)
(* Returns true if dimensions are valid *)
let rec ltyp_dim_trans (t1: ltyp) (t2: ltyp) (d: delta) : bool =
    debug_print ">> ltyp_dim_trans";
    match (t1, t2) with 
    | (VecTyp n1, VecTyp n2) -> true
    | (MatTyp (n1, n2), MatTyp (n3, n4)) -> n2 == n3
    | (TagTyp i1, TagTyp i2) -> 
        ltyp_dim_trans (Context.lookup d i1) (Context.lookup d i2) d
    | (TagTyp i1, _) -> ltyp_dim_trans (Context.lookup d i1) t2 d
    | (_, TagTyp i1) -> ltyp_dim_trans t1 (Context.lookup d i1) d
    | (TransTyp (lt1, lt2), TransTyp (lt3, lt4)) -> ltyp_dim_equals lt2 lt3 d
    | _ -> ltyp_dim_equals t1 t2 d

(* Type check linear types *)
let rec check_ltyp (lt: ltyp) (d: delta) : typ = 
    debug_print ">> check_ltyp";
    match lt with
    | VecTyp n -> if n < 0 
        then (raise (TypeException "vec dimensions must be positive"))
        else ATyp(LTyp(lt))
    | MatTyp (n1, n2) -> if n1 < 0 || n2 < 0 then
        (raise (TypeException "mat dimensions must be positive"))
        else ATyp(LTyp(lt))
    | TagTyp s -> let is_mem = Context.mem d s in 
        if not is_mem then (
            (raise (TypeException ("tag "^s^" must be defined")))
        ) else ATyp(LTyp(lt))
    | TransTyp (lt1, lt2) -> if ltyp_dim_trans lt1 lt2 d |> not
        then (raise (TypeException "transformation dimension mismatch"))
        else  ATyp(LTyp(lt))

(* Type check arithmetic types *)
let rec check_atyp (at: atyp) (d: delta) : typ = 
    debug_print ">> check_atyp";
    match at with
    | IntTyp
    | FloatTyp -> ATyp at
    | LTyp lt -> check_ltyp lt d

(* Type check types *)
let rec check_typ (t: typ) (d: delta) : typ = 
    debug_print ">> check_typ";
    match t with
    | ATyp at -> check_atyp at d
    | BTyp
    | UnitTyp -> t

(* Gets type of vector literals *)
let veclit_type (v: vec) : ltyp = 
    debug_print ">> veclit_type";
    VecTyp (List.length v)

(* Helper function for matrix literals
   Every vec in a mat needs to be of the same dimension  *)
let rec matlit_type_helper (m: mat) (dim: int) : bool =
    debug_print ">> matlit_type_helper";
    match m with
    | [] -> true
    | h::t -> dim = List.length h && matlit_type_helper t dim 
   
(* Gets type of matrix literals *)
let matlit_type (m: mat) : ltyp =  
    debug_print ">> matlit_type";
    match m with 
    | [] -> MatTyp(0,0)
    | _ -> let dim = (List.hd m |> List.length) in
        if matlit_type_helper m dim
        then (MatTyp(List.length m, dim))
        else (raise (TypeException "mat dimension inconsistent"))

(* Type checking arithmetic literals *)
let rec check_aval (av: avalue) (d: delta) : typ = 
    debug_print ">> check_aval";
    match av with
    | Num n -> ATyp(IntTyp)
    | Float f -> ATyp(FloatTyp)
    | VecLit (v, t) -> 
        let littyp = veclit_type v in 
        if is_subtype littyp t d then ATyp(LTyp(t)) 
        else (raise (TypeException "vec literal tag mismatch"))
    | MatLit (m, t) -> 
        let littyp = matlit_type m in 
        if is_subtype littyp t d then ATyp(LTyp(t)) 
        else (raise (TypeException ("mat literal tag mismatch :" ^(print_typ (ATyp(LTyp(t))))^", "^(print_typ (ATyp(LTyp(littyp)))))))

(* Finding least common "parent" for linear types, *)
(* Used for scalar binary operations on linear types *)
(* let least_common_parent (t1: ltyp) (t2: ltyp) (d: delta) : ltyp = 
    if is_subtype t1 t2 d then t2 
    else if is_subtype t2 t1 d then t1 
    else if ltyp_dim_equals t1 t2 d then ltyp_top_typ t1 d
    else (raise (TypeException ("cannot find least common parent"))) *)

(* Get list of ancestors for a linear type *)
(* Helper function for least_common_parent *)
let rec get_ancestor_list (t1: ltyp) (acc: ltyp list) (d: delta) : ltyp list =
    match t1 with 
    | VecTyp _ 
    | MatTyp _-> t1::(ltyp_top_typ t1 d)::acc
    | TagTyp i -> let t1' = (Context.lookup d i) in 
        get_ancestor_list t1' (t1::acc) d
    | _ -> failwith "FATAL ERROR - should not reach this line (get_ancestor of transtyp)"

(* Get last match in two lists *)
(* TODO - equals not defined for ltypes, the equality check probably does not work *)
let rec get_last_match (l1: 'a list) (l2: 'a list) (m: 'a): 'a =
    (* Printf.printf "%s" (m |> print_ltyp); *)
    match l1, l2 with
    | ([], []) -> m
    | (hd1::tl1, hd2::tl2) -> 
        if hd1 = hd2 then get_last_match tl1 tl2 hd1 else m 
    | _ -> m

(* Check equality between two ltyp's *)
let rec ltyp_equals (t1: ltyp) (t2: ltyp) (d: delta) : bool = 
    match (t1, t2) with 
    | (VecTyp n1, MatTyp(1, n2))
    | (MatTyp(1, n1), VecTyp n2) -> n1 = n2
    | (MatTyp _, MatTyp _)
    | (VecTyp _, VecTyp _) -> ltyp_dim_equals t1 t2 d
    | (TransTyp (l1, r1), TransTyp (l2, r2)) -> ltyp_equals l1 l2 d && ltyp_equals r1 r2 d
    | (TagTyp i1, TagTyp i2) -> ltyp_equals (Context.lookup d i1) (Context.lookup d i2) d
    | _ -> false

(* Find least common parent type between two linear types *)
let rec least_common_parent (t1: ltyp) (t2: ltyp) (d: delta) : ltyp = 
    match t1, t2 with 
    | (TransTyp (l1, r1), TransTyp (l2, r2)) -> 
        if ltyp_equals l1 l2 d && ltyp_equals r1 r2 d then t1
        else if ltyp_dim_equals t1 t2 d then ltyp_top_typ t1 d 
        else raise (TypeException("common parent type not found for TransTyp "))
    | (TransTyp _, _)
    | (_, TransTyp _) -> if ltyp_dim_equals t1 t2 d 
        then ltyp_top_typ t1 d else raise (TypeException("common parent type not found for TransTyp "))
    | _ -> let l1 = get_ancestor_list t1 [] d in 
        let l2 = get_ancestor_list t2 [] d in 
        get_last_match l1 l2 (List.hd l1)

(* Type checking binary operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_scalar_binop (t1: typ) (t2: typ) (d: delta) : typ =
    debug_print ">> check_scalar_binop";
    match (t1, t2) with 
    | (ATyp(IntTyp), ATyp(a))
    | (ATyp(a), ATyp(IntTyp)) 
    | (ATyp(FloatTyp), ATyp(a)) 
    | (ATyp(a), ATyp(FloatTyp)) -> ATyp a
    | (ATyp(LTyp a1), ATyp(LTyp a2)) -> ATyp(LTyp(least_common_parent a1 a2 d))
    | _ -> 
        (raise (TypeException ("invalid expressions for arithmetic operation: "^(print_typ t1)^", "^(print_typ t2))))

(* "scalar linear exp", (i.e. dot and ctimes) returns generalized MatTyp *)
let check_scalar_linear_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_scalar_linear_exp";
    match (t1, t2) with 
    | (ATyp(LTyp l1), ATyp(LTyp l2)) -> if ltyp_dim_equals l1 l2 d
        then ATyp(LTyp(ltyp_top_typ l1 d))
        else (raise (TypeException "dimension mismatch in dot/ctimes operator"))
    | _ -> (raise (TypeException "expected linear types for dot/ctimes operator"))

(* Type check norm expressions *)
let check_norm_exp (a: typ) (d: delta) : typ = 
    debug_print ">> check_norm_exp";
    match a with
    | ATyp(LTyp l) -> ATyp(FloatTyp)
    | _ -> (raise (TypeException "expected linear type for norm operator"))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_bool_binop";
    match (t1, t2) with 
    | (BTyp, BTyp) -> BTyp
    | _ -> raise (TypeException "expected boolean expression for binop")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t1: typ) : typ =
    debug_print ">> check_bool_unop";
    match t1 with 
    | BTyp -> BTyp
    | _ -> raise (TypeException "expected boolean expression for !")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only bool, int, float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_comp_binop";
    match (t1, t2) with
    | (BTyp, BTyp) -> BTyp
    | (ATyp(IntTyp), ATyp(IntTyp)) -> BTyp
    | (ATyp(FloatTyp), ATyp(FloatTyp)) -> BTyp
    | _ -> raise (TypeException "unexpected type for binary comparator operations")

let check_dot_exp (t1: typ) (t2: typ) : typ = 
    match (t1, t2) with 
    | (ATyp(LTyp _ ), ATyp(LTyp _)) -> ATyp(FloatTyp)
    | _ -> raise (TypeException "unexpected type for dot product exp")

(* Type checking times operator - on scalar mult & matrix transformations *)
let rec check_times_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_times_exp";
    match (t1, t2) with
    | (ATyp(LTyp(MatTyp(n1, n2))), ATyp(LTyp(MatTyp(n3, n4)))) -> 
        debug_print "\tmat, mat";
        if n2 = n3 then ATyp(LTyp(MatTyp(n1, n4))) 
        else (raise (TypeException "matrix multiplication dimension mismatch"))
    | (ATyp(LTyp(VecTyp(n1))), ATyp(LTyp(MatTyp(n3, n4)))) -> 
        debug_print "\tvec, mat";
        (raise (TypeException "vector * matrix is illegal"))
    | (ATyp(LTyp(MatTyp(n1, n2))), ATyp(LTyp(VecTyp(n3)))) -> 
        debug_print "\tmat, vec";
        if n2 = n3 then ATyp(LTyp(MatTyp(n1, 1))) 
        else (raise (TypeException "vector * matrix is illegal"))
    | (ATyp(LTyp(TransTyp(lt1, lt2))), ATyp(LTyp(TransTyp(lt3, lt4)))) ->
        debug_print "\ttrans, trans";
        if ltyp_dim_equals lt2 lt3 d then ATyp(LTyp(TransTyp(lt1, lt4)))
        else (raise (TypeException "linear transformation type mismatch"))
    | (ATyp(LTyp(TransTyp(lt1, lt2))), ATyp(LTyp(a))) ->
        debug_print "\ttrans, vec";
        if ltyp_dim_equals a lt1 d then ATyp(LTyp(lt2))
        else (raise (TypeException "linear transformation type mismatch"))

    | (ATyp(LTyp(TagTyp a1)), ATyp(LTyp(TagTyp a2))) ->
        let tagtyp1 = Context.lookup d a1 in 
        let tagtyp2 = Context.lookup d a2 in 
        check_times_exp (ATyp(LTyp(tagtyp1))) (ATyp(LTyp(tagtyp2))) d
    | (ATyp(LTyp(TagTyp a1)), ATyp a2) ->
        let tagtyp = Context.lookup d a1 in (
        check_times_exp (ATyp(LTyp(tagtyp))) t2 d
        )
    | (ATyp(a1), ATyp(LTyp(TagTyp a2))) ->
        let tagtyp = Context.lookup d a2 in (
        check_times_exp t1 (ATyp(LTyp(tagtyp))) d
        )
    | _ -> check_scalar_binop t1 t2 d

and check_exp (e: exp) (d: delta) (g: gamma) : typ = 
    debug_print ">> check_exp";
    match e with
    | Bool b -> BTyp
    | Aval a -> check_aval a d
    | Var v -> "\tVar "^v |> debug_print;
        Context.lookup g v
    | Norm a -> check_norm_exp (check_exp a d g) d
    | Dot (e1, e2) -> check_dot_exp (check_exp e1 d g) (check_exp e2 d g)
    | Plus (e1, e2)
    | Div (e1, e2)
    | Minus (e1, e2) -> check_scalar_binop (check_exp e1 d g) (check_exp e2 d g) d
    | Times (e1, e2) -> check_times_exp (check_exp e1 d g) (check_exp e2 d g) d
    | CTimes (e1, e2) -> check_scalar_linear_exp (check_exp e1 d g) (check_exp e2 d g) d
    | Eq (e1, e2)
    | Leq (e1, e2) -> check_comp_binop (check_exp e1 d g) (check_exp e2 d g)
    | Or (e1, e2)
    | And (e1, e2) -> check_bool_binop (check_exp e1 d g) (check_exp e2 d g)  
    | Not e1 -> check_bool_unop (check_exp e1 d g)
    | Typ typ -> check_typ typ d

let rec check_decl (t: typ) (s: string) (e: exp) (d: delta) (g: gamma) : delta * gamma =
    debug_print (">> check_decl <<"^s^">>");
    if Context.mem d s then 
        raise (TypeException "variable declared as tag")
    else (
        let etyp = check_exp e d g in
        let t' = check_typ t d in
        match (etyp, t') with
        | (ATyp(LTyp a1), ATyp(LTyp a2)) -> 
            if is_subtype a1 a2 d then (d, Context.update g s t')
            else raise (TypeException ("mismatched linear type for var decl: "^s))
        | (ATyp(IntTyp), ATyp(IntTyp))
        | (ATyp(FloatTyp), ATyp(FloatTyp))
        | (BTyp, BTyp) -> (d, Context.update g s t')
        | _ -> raise (TypeException "mismatched types for var decl")
    )

let rec check_comm (c: comm) (d: delta) (g: gamma) : delta * gamma = 
    debug_print ">> check_comm";
    match c with
    | Skip -> (d, g)
    | Print e -> ignore (check_exp e d g); (d, g)
    | Decl (t, s, e) -> check_decl t s e d g
    | If (b, c1, c2) -> check_comm_lst c1 d g |> ignore; check_comm_lst c2 d g |> ignore; 
        (match check_exp b d g with 
        | BTyp -> (d, g)
        | _ -> raise (TypeException "expected boolean expression for if condition"))

and check_comm_lst (cl : comm list) (d: delta) (g: gamma): delta * gamma = 
    debug_print ">> check_comm_lst";
    match cl with
    | [] -> (d, g)
    | h::t -> let context = check_comm h d g
        in check_comm_lst t (fst context) (snd context)

let rec check_tags (t : tagdecl list) (d: delta): delta =
    debug_print ">> check_tags";
    match t with 
    | [] -> d
    | (s, a)::t -> 
        ignore (check_atyp a d);
        match a with 
        | (LTyp l) -> 
            Context.update d s l |> check_tags t
        | _ -> raise (TypeException "expected linear type for tag declaration")

let check_prog (e : prog) : unit =
    debug_print ">> check_prog";
    match e with
    | Prog (t, c) -> let d = Context.empty in 
        let d' = check_tags t d in check_comm_lst c d' Context.empty |> ignore