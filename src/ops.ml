open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Glsl_ops
open Util

type sigma = (value) Assoc.context

let rec fn_lookup (name : id) (fns : fn list) : (fn option * id list) =
    match fns with
    | [] -> (None, [])
    | h::t -> match h with ((id, (p, _)), _) -> 
        (if name = id then (Some h, List.map fst (Assoc.bindings p)) else fn_lookup name t)

let rec eval_glsl_fn (name : id) (args : exp list) (fns : fn list) (s : sigma) : value =
        let as_vec (e : exp) : vec =
            (match (eval_exp e fns s) with
            | VecLit v -> v
            | _ -> failwith "Expected vec, but didn't get it!")
        in
        if name = "dot" then Float (dot (as_vec (List.nth args 0)) (as_vec (List.nth args 1))) else
        if name = "normalize" then VecLit (normalize (as_vec (List.nth args 0))) else
        if Str.string_match (Str.regexp "vec[0-9]+") name 0 then
            VecLit (vecn (int_of_string (Str.string_after name 3))
            (List.map (fun e -> eval_exp e fns s) args)) else
        if Str.string_match (Str.regexp "mat[0-9]+") name 0 then
            MatLit (matn (int_of_string (Str.string_after name 3))
            (List.map (fun e -> eval_exp e fns s) args)) else
        failwith ("Unimplemented function " ^ name ^ " -- is this a GLSL function?")

and eval_exp (e : exp) (fns : fn list) (s : sigma) : value =
    match e with
    | Val v -> v
    | Var x -> Assoc.lookup x s
    | Arr a -> 
        let result = (List.map (fun e' -> eval_exp e' fns s) (List.map fst a)) in
        let rec as_vec (arr : value list) : vec option =
            (match arr with
            | [] -> Some []
            | h::t -> 
            (match h with | Float f -> (option_map (fun l -> f::l) (as_vec t)) | _ -> None ))
        in
        let rec as_mat (arr : value list) : mat option =
            (match arr with
            | [] -> Some []
            | h::t -> 
            (match h with | VecLit v -> (option_map (fun l -> v::l) (as_mat t)) | _ -> None ))
        in
        (match as_vec result with
        | Some v -> VecLit v
        | None -> (match as_mat result with
            | Some m -> MatLit m
            | None -> failwith ("Typechecker failure, bad arr " ^ (string_of_exp e))))
    | Unop (op, (e', _)) ->
        let v = eval_exp e' fns s in
        let bad_unop _ =
            failwith ("No rule to apply " ^ (string_of_unop op (string_of_value v)))
        in
        (match op with
        | Neg -> (match v with
            | Num i -> Num (-i)
            | Float f -> Float (-.f)
            | VecLit v -> VecLit (List.map (~-.) v)
            | MatLit m -> MatLit (List.map (fun v -> (List.map (~-.) v)) m)
            | _ -> bad_unop ())
        | Not -> (match v with
            | Bool b -> Bool (not b)
            | _ -> bad_unop ())
        | Swizzle s -> (match v with
            | VecLit v -> let res = swizzle s v in
                if List.length res == 1 then Float (List.hd res) else VecLit res
            | _ -> bad_unop ()))

    | Binop (op, (l, _), (r, _)) -> 
        let left = eval_exp l fns s in
        let right = eval_exp r fns s in
        let bad_binop _ =
            failwith ("No rule to apply " ^ 
            (string_of_binop op (string_of_value left) (string_of_value right)))
        in
        (match op with
        | Eq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 = n2)
            | (Float f1, Float f2) -> Bool (f1 = f2)
            | (Bool b1, Bool b2) -> Bool (b1 = b2)
            | _ -> bad_binop ())
        | Leq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 <= n2)
            | (Float f1, Float f2) -> Bool (f1 <= f2)
            | _ -> bad_binop ())
        | Lt -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 < n2)
            | (Float f1, Float f2) -> Bool (f1 < f2)
            | _ -> bad_binop ())
        | Geq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 >= n2)
            | (Float f1, Float f2) -> Bool (f1 >= f2)
            | _ -> bad_binop ())
        | Gt -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 > n2)
            | (Float f1, Float f2) -> Bool (f1 > f2)
            | _ -> bad_binop ())
        | Or -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 || b2)
            | _ -> bad_binop ())
        | And -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 && b2)
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
            | (VecLit v, Num n)
            | (Num n, VecLit v) -> VecLit (iv_mult n v)
            | (VecLit v, Float s)
            | (Float s, VecLit v) -> VecLit (sv_mult s v)
            | (MatLit m, Num n)
            | (Num n, MatLit m) -> MatLit (im_mult n m)
            | (MatLit m, Float s)
            | (Float s, MatLit m) -> MatLit (sm_mult s m)
            | (MatLit m, VecLit v) -> VecLit (vec_mult v m)
            | (MatLit m1, MatLit m2) -> MatLit (mat_mult m1 m2)
            | _ -> bad_binop ())

        | Div -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 / i2)
            | (Float f1, Float f2) -> Float (f1 /. f2)
            | (VecLit v, Num n) -> VecLit (sv_mult (1. /. (float_of_int n)) v)
            | (VecLit v, Float s) -> VecLit (sv_mult (1. /. s) v)
            | (MatLit m, Num n) -> MatLit (sm_mult (1. /. (float_of_int n)) m)
            | (MatLit m, Float s) -> MatLit (sm_mult (1. /. s) m)
            | _ -> bad_binop ())

        | CTimes -> (match (left, right) with
            | (VecLit v1, VecLit v2) -> VecLit (vc_mult v1 v2)
            | (MatLit m1, MatLit m2) -> MatLit (mc_mult m1 m2)
            | _ -> bad_binop ())
        | Index -> (match (left, right) with
            | (VecLit v, Num i) -> Float (List.nth v i)
            | (MatLit m, Num i) -> VecLit (List.map (fun v -> List.nth v i) m)
            | _ -> bad_binop ())
        )
    | FnInv (id, args) -> let (fn, p) = fn_lookup id fns in
        match fn with
        | None -> eval_glsl_fn id args fns s
        | Some f -> (match f with ((_, (names, _)), _) ->
            let add_arg = (fun acc (name, _) ex -> Assoc.update name (eval_exp ex fns s) acc) in
            eval_funct f fns (List.fold_left2 (add_arg) Assoc.empty (Assoc.bindings names) args))

and eval_comm (c : comm) (fns : fn list) (s : sigma) : sigma =
    match c with
    | Skip -> s
    | Print (e, _) -> print_string (string_of_value (eval_exp e fns s) ^ "\n"); s
    | Inc (x, t) -> let x_val = Assoc.lookup x s in (match (x_val, t) with
        | (Num n, IntTyp) -> Assoc.update x (Num (n + 1)) s 
        | (Float f, FloatTyp) -> Assoc.update x (Float (f +. 1.)) s
        | _ -> failwith "Typchecker error: cannot apply inc to a non-int/float type")
    | Dec (x, t) -> let x_val = Assoc.lookup x s in (match (x_val, t) with
        | (Num n, IntTyp) -> Assoc.update x (Num (n - 1)) s 
        | (Float f, FloatTyp) -> Assoc.update x (Float (f -. 1.)) s
        | _ -> failwith "Typchecker error: cannot apply dec to a non-int/float type")
    | Decl (_, x, (e, _))
    | Assign (x, (e, _)) -> Assoc.update x (eval_exp e fns s) s
    | AssignOp ((x, xt), op, e) -> Assoc.update x (eval_exp (TypedAst.Binop (op, ((TypedAst.Var x), xt), e)) fns s) s
    | If (((b, _), c1), el, c2) ->
        let check_if b = (match (eval_exp b fns s) with
        | Bool b -> b
        | _ -> failwith ("Typechecker failure: expected a boolean in 'if' expression"))
        in
        let check_cmd b c acc = (match acc with
        | Some _ -> acc
        | None -> if (check_if b) then Some (snd (eval_cl c fns s)) else None)
        in
        (match List.fold_left (fun acc ((b, _), c) -> check_cmd b c acc) (check_cmd b c1 None) el with
        | Some s' -> s'
        | None -> (match c2 with | Some c -> (snd (eval_cl c fns s)) | None -> s))
    | For (c1, (b, _), c2, cl) ->
        let rec loop s = (match (eval_exp b fns s) with
        | Bool br -> if br then loop (eval_comm c2 fns (snd (eval_cl cl fns s))) else s
        | _ -> failwith "Typechecker failure -- expected a boolean expression")
        in
        loop (eval_comm c1 fns s)
    | Return e -> s
    | FnCall (id, args) -> (eval_exp (FnInv (id, args))) |> ignore; s

and eval_cl (cl : comm list) (fns : fn list) (s : sigma) : (value * sigma) =
    match cl with
    | [] -> (Unit, s)
    | h::t -> (match h with
        | Return Some (e, _) -> (eval_exp e fns s, s)
        | Return None -> (Unit, s)
        | _ -> eval_cl t fns (eval_comm h fns s))

and eval_funct (((p, rt), cl) : fn) (fns : fn list) (s : sigma) : value =
    fst (eval_cl cl fns s)
    
let start_eval (fns : fn list) : unit =
    match fst (fn_lookup "main" fns) with
    | None -> failwith "Typechecker failed to find lack of main"
    | Some main -> eval_funct main fns Assoc.empty |> ignore

let eval_prog (p : prog) : unit =
    start_eval p