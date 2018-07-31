open CoreAst
open CoreAstHelper
open TypedAst
open Assoc
open Lin_ops
open Util

type sigma = (id, value) Assoc.context

let rec eval_exp (e : exp) (s : sigma) : value =
    match e with
    | Val v -> v
    | Var x -> Assoc.lookup x s
    | Unop (op, (e', _)) ->
        let v = eval_exp e' s in
        let bad_unop _ =
            failwith ("No rule to apply " ^ (string_of_unop op (string_of_value v)))
        in
        (match op with
        | Not -> (match v with
            | Bool b -> Bool (not b)
            | _ -> bad_unop ())
        | Norm -> (match v with
            | VecLit v -> VecLit (normalize v)
            | _ -> bad_unop ()))

    | Binop (op, (l, _), (r, _)) -> 
        let left = eval_exp l s in
        let right = eval_exp r s in
        let bad_binop _ =
            failwith ("No rule to apply " ^ 
            (string_of_binop op (string_of_value left) (string_of_value right)))
        in
        (match op with
        | Eq -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 = b2)
            | (Num n1, Num n2) -> Bool (n1 = n2)
            | (Float f1, Float f2) -> Bool (f1 = f2)
            | _ -> bad_binop ())
        | Leq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 <= n2)
            | (Float f1, Float f2) -> Bool (f1 <= f2)
            | _ -> bad_binop ())
        | Or -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 || b2)
            | _ -> bad_binop ())
        | And -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 && b2)
            | _ -> bad_binop ())

        | Dot -> (match (left, right) with
            | (VecLit v1, VecLit v2) -> Float (dot v1 v2)
            | _ -> bad_binop ())
            
        | Plus -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 + i2)
            | (Float f1, Float f2) -> Float (f1 +. f2)
            | (VecLit v1, VecLit v2) -> VecLit (vec_add v1 v2)
            | (MatLit m1, MatLit m2) -> MatLit (mat_add m1 m2)
            | _ -> bad_binop ())

        | Minus -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 - i2)
            | (Float f1, Float f2) -> Float (f1 -. f2)
            | (VecLit v1, VecLit v2) -> VecLit (vec_sub v1 v2)
            | (MatLit m1, MatLit m2) -> MatLit (mat_sub m1 m2)
            | _ -> bad_binop ())

        | Times -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 * i2)
            | (Float f1, Float f2) -> Float (f1 *. f2)
            | (VecLit v, Float s) -> VecLit (sv_mult s v)
            | (Float s, VecLit v) -> VecLit (sv_mult s v)
            | (MatLit m, Float s) -> MatLit (sm_mult s m)
            | (Float s, MatLit m) -> MatLit (sm_mult s m)
            | (MatLit m, VecLit v) -> VecLit (vec_mult v m)
            | (MatLit m1, MatLit m2) -> MatLit (mat_mult m1 m2)
            | _ -> bad_binop ())

        | Div -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 / i2)
            | (Float f1, Float f2) -> Float (f1 /. f2)
            | (VecLit v, Float s) -> VecLit (sv_mult (1. /. s) v)
            | (MatLit m, Float s) -> MatLit (sm_mult (1. /. s) m)
            | _ -> bad_binop ())

        | CTimes -> (match (left, right) with
            | (VecLit v1, VecLit v2) -> VecLit (vc_mult v1 v2)
            | (MatLit m1, MatLit m2) -> MatLit (mc_mult m1 m2)
            | _ -> bad_binop ()))

let rec eval_comm (c : comm list) (s : sigma) : sigma =
    match c with
    | [] -> s
    | h::t -> eval_comm t (match h with
        | Skip -> s
        | Print (e, _) -> print_string (string_of_value (eval_exp e s) ^ "\n"); s
        | Decl (_, x, (e, _))
        | Assign (x, (e, _)) -> Assoc.update x (eval_exp e s) s
        | If ((e, _), c1, c2) -> eval_comm (match (eval_exp e s) with
            | Bool b -> if b then c1 else c2
            | _ -> failwith "Expected a boolean in 'if' exception") s)

let eval_prog (p : prog) : unit =
    eval_comm p Assoc.empty |> ignore