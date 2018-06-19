open Ast
open Context
open Lin_ops

type sigma = value context

let rec eval_aexp (e : exp) (s : sigma) : avalue =
    match e with
    | Aval a -> a
    | Var x -> (match (Context.lookup s x) with
        | Avalue a -> a
        | Bvalue b -> failwith ("Invalid use of non-avalue " ^ x))
    | Dot (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (VecLit (v1, _), VecLit (v2, _)) -> Float (dot v1 v2)
        | _ -> failwith "Invalid dot product")

    | Norm a -> (match (eval_aexp a s) with
        | VecLit (v, _) -> Float (norm v)
        | _ -> failwith "Invalid norm")

    | Plus (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 + i2)
        | (Float f1, Float f2) -> Float (f1 +. f2)
        | (VecLit (v1, t), VecLit (v2, _)) -> VecLit (vec_add v1 v2, t)
        | (MatLit (m1, t), MatLit (m2, _)) -> MatLit (mat_add m1 m2, t)
        | _ -> failwith "Invalid addition")

    | Times (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 * i2)
        | (Float f1, Float f2) -> Float (f1 *. f2)
        | (VecLit (v, t), Float s) -> VecLit (sv_mult s v, t)
        | (Float s, VecLit (v, t)) -> VecLit (sv_mult s v, t)
        | (MatLit (m, t), Float s) -> MatLit (sm_mult s m, t)
        | (Float s, MatLit (m, t)) -> MatLit (sm_mult s m, t)
        | (VecLit (v, t), MatLit (m, _)) -> VecLit (vec_mult v m, t)
        | (MatLit (m1, t), MatLit (m2, _)) -> MatLit (mat_mult m1 m2, t)
        | _ -> failwith "Invalid multiplication")

    | Minus (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> Num (i1 - i2)
        | (Float f1, Float f2) -> Float (f1 -. f2)
        | (VecLit (v1, t), VecLit (v2, _)) -> VecLit (vec_sub v1 v2, t)
        | (MatLit (m1, t), MatLit (m2, _)) -> MatLit (mat_sub m1 m2, t)
        | _ -> failwith "Invalid subtraction")

    | CTimes (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (VecLit (v1, t), VecLit (v2, _)) -> VecLit (vc_mult v1 v2, t)
        | (MatLit (m1, t), MatLit (m2, _)) -> MatLit (mc_mult m1 m2, t)
        | _ -> failwith "Invalid component multiplication")
    | _ -> failwith "Not an arithmetic expression"

let rec eval_bexp (e : exp) (s : sigma) : bvalue =
    match e with
    | Bool b -> b 
    | Var x -> (match (Context.lookup s x) with
        | Avalue a -> failwith ("Invalid use of non-bvalue " ^ x)
        | Bvalue b -> b)
    | Eq (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> i1 = i2
        | (Float f1, Float f2) -> f1 = f2
        | (VecLit (v1, _), VecLit (v2, _)) -> vec_eq v1 v2
        | (MatLit (m1, _), MatLit (m2, _)) -> mat_eq m1 m2
        | _ -> false)
    | Leq (a1, a2) -> (match ((eval_aexp a1 s), (eval_aexp a2 s)) with
        | (Num i1, Num i2) -> i1 <= i2
        | (Float f1, Float f2) -> f1 <= f2
        | _ -> failwith "Invalid comparison")
    | Or (b1, b2) -> (eval_bexp b1 s) || (eval_bexp b2 s)
    | And (b1, b2) -> (eval_bexp b1 s) && (eval_bexp b2 s)
    | Not b -> not (eval_bexp b s)
    | _ -> failwith "Not a boolean expression"

let string_of_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let string_of_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map string_of_vec m))^"]"

let string_of_avalue (a : avalue) : string =
    match a with 
    | Num i -> string_of_int i
    | Float f -> string_of_float f
    | VecLit (v, _) -> string_of_vec v 
    | MatLit (m, _) -> string_of_mat m

let string_of_value (v : value) : string =
    match v with
    | Avalue a -> string_of_avalue a
    | Bvalue b -> string_of_bool b

let eval_print (e : exp) (s : sigma) : string =
    match e with
    | Var v -> v ^ " = " ^ (string_of_value (lookup s v))
    | _ -> (try string_of_avalue (eval_aexp e s) with
        | Failure "Not an arithmetic expression" -> (try string_of_bool (eval_bexp e s) with
            | Failure s -> failwith s)
        | Failure s -> failwith s)

let eval_declare (x : id) (e : exp) (s : sigma) : value =
    match e with 
    | Var v -> Context.lookup s v
    | _ -> (try Avalue (eval_aexp e s) with
        | Failure "Not an arithmetic expression" -> (try Bvalue (eval_bexp e s) with
            | Failure s -> failwith s)
        | Failure s -> failwith s)

let rec eval_comm (c : comm list) (s : sigma) : sigma =
    match c with
    | [] -> s
    | h::t -> eval_comm t (match h with
        | Skip -> s
        | Print e -> print_string ((eval_print e s) ^ "\n"); s
        | Decl (_, x, e) -> Context.update s x (eval_declare x e s)
        | If (e, c1, c2) -> (match e with 
            | Var v -> (match (Context.lookup s v) with
                | Avalue a -> failwith "Bad if condition"
                | Bvalue b -> if b then (eval_comm c1 s) else (eval_comm c2 s))
            | _ -> (try (if (eval_bexp e s) then (eval_comm c1 s) else (eval_comm c2 s)) with
                | Failure s -> failwith s)))

let rec eval_prog (p : prog) : unit =
    match p with
    | Prog (_, c) -> let _ = eval_comm c Context.empty in ()