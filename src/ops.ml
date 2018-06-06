open Ast
open State
open Lin_ops

let rec eval_aexp (e : aexp) (s : state) : avalue =
    match e with
    | Const c -> c
    | Var x -> (match (State.lookup s x) with
        | Avalue a -> a
        | Bvalue b -> failwith ("Invalid use of non-avalue " ^ x))
    | Lexp (a, _) -> eval_aexp a s
    | Dot (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (VecLit v1, VecLit v2) -> Float (dot v1 v2)
        | _ -> failwith "Invalid dot product")

    | Norm a -> (match (eval_aexp a s) with
        | VecLit v -> Float (norm v)
        | _ -> failwith "Invalid norm")

    | Plus (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 + i2)
        | (Float f1, Float f2) -> Float (f1 +. f2)
        | (VecLit v1, VecLit v2) -> VecLit (vec_add v1 v2)
        | (MatLit m1, MatLit m2) -> MatLit (mat_add m1 m2)
        | _ -> failwith "Invalid addition")

    | Times (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 * i2)
        | (Float f1, Float f2) -> Float (f1 *. f2)
        | (VecLit v, Float s) -> VecLit (sv_mult s v)
        | (Float s, VecLit v) -> VecLit (sv_mult s v)
        | (MatLit m, Float s) -> MatLit (sm_mult s m)
        | (Float s, MatLit m) -> MatLit (sm_mult s m)
        | (VecLit v, MatLit m) -> VecLit (vec_mult v m)
        | (MatLit m1, MatLit m2) -> MatLit (mat_mult m1 m2)
        | _ -> failwith "Invalid multiplication")

    | Minus (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 - i2)
        | (Float f1, Float f2) -> Float (f1 -. f2)
        | (VecLit v1, VecLit v2) -> VecLit (vec_sub v1 v2)
        | (MatLit m1, MatLit m2) -> MatLit (mat_sub m1 m2)
        | _ -> failwith "Invalid subtraction")

    | CTimes (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (VecLit v1, VecLit v2) -> VecLit (vc_mult v1 v2)
        | (MatLit m1, MatLit m2) -> MatLit (mc_mult m1 m2)
        | _ -> failwith "Invalid component multiplication")

let rec eval_bexp (e : bexp) (s : state) : bvalue =
    match e with
    | True -> true 
    | False -> false
    | Var x -> (match (State.lookup s x) with
        | Avalue a -> failwith ("Invalid use of non-bvalue " ^ x)
        | Bvalue b -> b)
    | Eq (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> i1 = i2
        | (Float f1, Float f2) -> f1 = f2
        | (VecLit v1, VecLit v2) -> vec_eq v1 v2
        | (MatLit m1, MatLit m2) -> mat_eq m1 m2
        | _ -> false)
    | Leq (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> i1 <= i2
        | (Float f1, Float f2) -> f1 <= f2
        | _ -> failwith "Invalid comparison")
    | Or (b1, b2) -> (eval_bexp b1 s) || (eval_bexp b2 s)
    | And (b1, b2) -> (eval_bexp b1 s) && (eval_bexp b2 s)
    | Not b -> not (eval_bexp b s)

let vec_to_string (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let mat_to_string (m: mat) : string = 
    "["^(String.concat ", " (List.map vec_to_string m))^"]"

let string_of_avalue (a : avalue) : string =
    match a with 
    | Num i -> string_of_int i
    | Float f -> string_of_float f
    | VecLit v -> vec_to_string v 
    | MatLit m -> mat_to_string m

let string_of_value (v : value) : string =
    match v with
    | Avalue a -> string_of_avalue a
    | Bvalue b -> string_of_bool b

let eval_print (e : exp) (s : state) : string =
    match e with
    | Aexp a -> string_of_avalue (eval_aexp a s)
    | Bexp b -> string_of_bool (eval_bexp b s)
    | Var v -> v ^ (string_of_value (lookup s v))

let eval_declare (x : id) (e : exp) (s : state) : value =
    match e with 
    | Aexp a -> Avalue (eval_aexp a s)
    | Bexp b -> Bvalue (eval_bexp b s)
    | Var v -> lookup s v

let rec eval_comm (c : comm list) (s : state) : state =
    match c with
    | [] -> s
    | h::t -> eval_comm t (match h with
        | Skip -> s
        | Print e -> print_string ((eval_print e s) ^ "\n"); s
        | Decl (_, x, e) -> update s x (eval_declare x e s)
        | If (e, c1, c2) -> (match e with 
            | Aexp a -> failwith "Bad if condition"
            | Bexp b -> if (eval_bexp b s) then (eval_comm c1 s) else (eval_comm c2 s)
            | Var v -> match (lookup s v) with
                | Avalue a -> failwith "Bad if condition"
                | Bvalue b -> if b then (eval_comm c1 s) else (eval_comm c2 s)))

let rec eval_prog (p : prog) : unit =
    match p with
    | Prog (_, c) -> let _ = eval_comm c (make ()) in ()