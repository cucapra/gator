open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Glsl_ops
open Util

type sigma = (value) Assoc.context

let interp_string_of_vec (v: vec) : string = 
    "[" ^ (String.concat ", " (List.map string_of_float v)) ^ "]"
  
let interp_string_of_mat (m: mat) : string = 
    "[" ^ (String.concat ", " (List.map interp_string_of_vec m)) ^ "]"

let rec fn_lookup (name : id) (fns : fn list) : (fn option * id list) =
    match fns with
    | [] -> (None, [])
    | h::t -> match h with ((id, (p, _, _)), _) -> 
        (if name = id then (Some h, List.map snd p) else fn_lookup name t)

let rec eval_glsl_fn (name : id) (args : value list) : value =
        let as_vec (v : value) : vec =
            (match v with
            | VecLit vec -> vec
            | _ -> failwith "Expected vec, but didn't get it!")
        in
        if name = "dot" then Float (dot (as_vec (List.nth args 0)) (as_vec (List.nth args 1))) else
        if name = "normalize" then VecLit (normalize (as_vec (List.nth args 0))) else
        if Str.string_match (Str.regexp "vec[0-9]+") name 0 then
            VecLit (vecn (int_of_string (Str.string_after name 3)) args) else
        if Str.string_match (Str.regexp "mat[0-9]+") name 0 then
            MatLit (matn (int_of_string (Str.string_after name 3)) args) else
        failwith ("Unimplemented function " ^ name ^ " -- is this a GLSL function?")

and eval_texp ((e, _) : texp) (fns : fn list) (s : sigma) (s_g : sigma) : value * sigma =
    eval_exp e fns s s_g
and eval_exp (e : exp) (fns : fn list) (s : sigma) (s_g : sigma) : value * sigma =
    match e with
    | Val v -> v, s_g
    | Var x -> (try Assoc.lookup x s with _ -> Assoc.lookup x s_g), s_g
    | Arr a ->
        let (result, s_g') = (List.fold_left (fun (vl, s_g) (e, _) -> let (v, s_g) = eval_exp e fns s s_g in vl@[v], s_g) ([], s_g) a) in
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
        | Some v -> VecLit v, s_g'
        | None -> (match as_mat result with
            | Some m -> MatLit m, s_g'
            | None -> failwith ("Typechecker failure, bad arr " ^ (string_of_exp e))))
    | Unop (op, e') ->
        let (v, s_g') = eval_texp e' fns s s_g in
        let bad_unop _ =
            failwith ("No rule to apply " ^ (string_of_unop_exp op (string_of_value v)))
        in
        (match op with
        | Neg -> (match v with
            | Num i -> Num (-i), s_g'
            | Float f -> Float (-.f), s_g'
            | VecLit v -> VecLit (List.map (~-.) v), s_g'
            | MatLit m -> MatLit (List.map (fun v -> (List.map (~-.) v)) m), s_g'
            | _ -> bad_unop ())
        | Not -> (match v with
            | Bool b -> Bool (not b), s_g'
            | _ -> bad_unop ())
        | Swizzle s -> (match v with
            | VecLit v -> let res = swizzle s v in
                if List.length res == 1 then Float (List.hd res), s_g' else VecLit res, s_g'
            | _ -> bad_unop ()))

    | Binop (l, op, r) -> 
        let (left, s_g') = eval_texp l fns s s_g in
        let (right, s_g'') = eval_texp r fns s s_g' in
        let bad_binop _ =
            failwith ("No rule to apply " ^ 
            (string_of_binop_exp (string_of_value left) op (string_of_value right)))
        in
        (match op with
        | Eq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 = n2), s_g''
            | (Float f1, Float f2) -> Bool (f1 = f2), s_g''
            | (Bool b1, Bool b2) -> Bool (b1 = b2), s_g''
            | _ -> bad_binop ())
        | Leq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 <= n2), s_g''
            | (Float f1, Float f2) -> Bool (f1 <= f2), s_g''
            | _ -> bad_binop ())
        | Lt -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 < n2), s_g''
            | (Float f1, Float f2) -> Bool (f1 < f2), s_g''
            | _ -> bad_binop ())
        | Geq -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 >= n2), s_g''
            | (Float f1, Float f2) -> Bool (f1 >= f2), s_g''
            | _ -> bad_binop ())
        | Gt -> (match (left, right) with
            | (Num n1, Num n2) -> Bool (n1 > n2), s_g''
            | (Float f1, Float f2) -> Bool (f1 > f2), s_g''
            | _ -> bad_binop ())
        | Or -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 || b2), s_g''
            | _ -> bad_binop ())
        | And -> (match (left, right) with
            | (Bool b1, Bool b2) -> Bool (b1 && b2), s_g''
            | _ -> bad_binop ())            
        | Plus -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 + i2), s_g''
            | (Float f1, Float f2) -> Float (f1 +. f2), s_g''
            | (VecLit v1, VecLit v2) -> VecLit (vec_add v1 v2), s_g''
            | (MatLit m1, MatLit m2) -> MatLit (mat_add m1 m2), s_g''
            | _ -> bad_binop ())

        | Minus -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 - i2), s_g''
            | (Float f1, Float f2) -> Float (f1 -. f2), s_g''
            | (VecLit v1, VecLit v2) -> VecLit (vec_sub v1 v2), s_g''
            | (MatLit m1, MatLit m2) -> MatLit (mat_sub m1 m2), s_g''
            | _ -> bad_binop ())

        | Times -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 * i2), s_g''
            | (Float f1, Float f2) -> Float (f1 *. f2), s_g''
            | (VecLit v, Num n)
            | (Num n, VecLit v) -> VecLit (iv_mult n v), s_g''
            | (VecLit v, Float s)
            | (Float s, VecLit v) -> VecLit (sv_mult s v), s_g''
            | (MatLit m, Num n)
            | (Num n, MatLit m) -> MatLit (im_mult n m), s_g''
            | (MatLit m, Float s)
            | (Float s, MatLit m) -> MatLit (sm_mult s m), s_g''
            | (MatLit m, VecLit v) -> VecLit (vec_mult v m), s_g''
            | (MatLit m1, MatLit m2) -> MatLit (mat_mult m1 m2), s_g''
            | _ -> bad_binop ())

        | Div -> (match (left, right) with
            | (Num i1, Num i2) -> Num (i1 / i2), s_g''
            | (Float f1, Float f2) -> Float (f1 /. f2), s_g''
            | (VecLit v, Num n) -> VecLit (sv_mult (1. /. (float_of_int n)) v), s_g''
            | (VecLit v, Float s) -> VecLit (sv_mult (1. /. s) v), s_g''
            | (MatLit m, Num n) -> MatLit (sm_mult (1. /. (float_of_int n)) m), s_g''
            | (MatLit m, Float s) -> MatLit (sm_mult (1. /. s) m), s_g''
            | _ -> bad_binop ())

        | CTimes -> (match (left, right) with
            | (VecLit v1, VecLit v2) -> VecLit (vc_mult v1 v2), s_g''
            | (MatLit m1, MatLit m2) -> MatLit (mc_mult m1 m2), s_g''
            | _ -> bad_binop ())
        | Index -> (match (left, right) with
            | (VecLit v, Num i) -> Float (List.nth v i), s_g''
            | (MatLit m, Num i) -> VecLit (List.map (fun v -> List.nth v i) m), s_g''
            | _ -> bad_binop ())
        )
    | FnInv (id, tl, args) -> let (fn, p) = fn_lookup id fns in
        let (arg_vs, s_g') = (List.fold_left (fun (vl, s_g) (e, _) -> let (v, s_g) = eval_exp e fns s s_g in vl@[v], s_g) ([], s_g) args) in
        match fn with
        | None -> eval_glsl_fn id arg_vs, s_g'
        | Some f -> (match f with ((_, (names, _, _)), _) ->
            let add_arg = (fun acc (_, name) v -> Assoc.update name v acc) in
            eval_funct f fns (List.fold_left2 add_arg Assoc.empty names arg_vs)) s_g'

and eval_comm (c : comm) (fns : fn list) (s : sigma) (s_g : sigma) : sigma * sigma =
    match c with
    | Skip -> s, s_g
    | Print (e, _) -> let v, s_g = eval_exp e fns s s_g in
        (print_string ((match v with 
            | VecLit v -> interp_string_of_vec v
            | MatLit m -> interp_string_of_mat m
            | _ -> (string_of_value v)
            ) ^ "\n"));
        s, s_g
    | Inc (x, t) -> (try let x_val = Assoc.lookup x s in (match (x_val, t) with
            | (Num n, IntTyp) -> Assoc.update x (Num (n + 1)) s, s_g
            | (Float f, FloatTyp) -> Assoc.update x (Float (f +. 1.)) s, s_g
            | _ -> failwith "Typchecker error: cannot apply inc to a non-int/float type") with
        _ -> let x_val = Assoc.lookup x s_g in (match (x_val, t) with
            | (Num n, IntTyp) -> s, Assoc.update x (Num (n + 1)) s_g
            | (Float f, FloatTyp) -> s, Assoc.update x (Float (f +. 1.)) s_g
            | _ -> failwith "Typchecker error: cannot apply inc to a non-int/float type"))
    | Dec (x, t) -> (try let x_val = Assoc.lookup x s in (match (x_val, t) with
            | (Num n, IntTyp) -> Assoc.update x (Num (n - 1)) s, s_g
            | (Float f, FloatTyp) -> Assoc.update x (Float (f -. 1.)) s, s_g
            | _ -> failwith "Typchecker error: cannot apply inc to a non-int/float type") with
        _ -> let x_val = Assoc.lookup x s_g in (match (x_val, t) with
            | (Num n, IntTyp) -> s, Assoc.update x (Num (n - 1)) s_g
            | (Float f, FloatTyp) -> s, Assoc.update x (Float (f -. 1.)) s_g
            | _ -> failwith "Typchecker error: cannot apply inc to a non-int/float type"))
    | Decl (_, x, (e, _))
    | Assign (x, (e, _)) -> let v, s_g' = eval_exp e fns s s_g in
        (try let _ = Assoc.lookup x s_g in s, Assoc.update x v s_g' with
        _ -> Assoc.update x v s, s_g')
    | AssignOp ((x, xt), op, e) -> let v, s_g' = eval_exp (TypedAst.Binop (((TypedAst.Var x), xt), op, e)) fns s s_g in
        (try let _ = Assoc.lookup x s_g in s, Assoc.update x v s_g' with
        _ -> Assoc.update x v s, s_g')
    | If (((b, _), c1), el, c2) ->
        let check_if b s_g = (match (eval_exp b fns s s_g) with
        | Bool b', s_g' -> b', s_g'
        | _ -> failwith ("Typechecker failure: expected a boolean in 'if' expression"))
        in
        let check_cmd b c acc = (match acc with
            | Some s, _ -> acc
            | None, s_g -> let v, s_g' = check_if b s_g in
                if v then let _, s', s_g'' = eval_cl c fns s s_g in Some s', s_g''
                else None, s_g')
        in
        (match List.fold_left (fun acc ((b, _), c) -> check_cmd b c acc) (check_cmd b c1 (None, s_g)) el with
        | Some s', s_g' -> s', s_g'
        | None, s_g' -> (match c2 with | Some c -> (let _, s', s_g'' = eval_cl c fns s s_g' in s', s_g'') | None -> s, s_g'))
    | For (c1, (b, _), c2, cl) ->
        let rec loop (s, s_g) = (match (eval_exp b fns s s_g) with
        | Bool br, s_g ->
            if br then
                let _, s', s_g' = eval_cl cl fns s s_g in
                loop (eval_comm c2 fns s' s_g')
            else s, s_g
        | _ -> failwith "Typechecker failure -- expected a boolean expression")
        in
        loop (eval_comm c1 fns s s_g)
    | Return e -> s, s_g
    | FnCall (id, tl, args) -> let v, s_g' = eval_exp (FnInv (id, tl, args)) fns s s_g  in s, s_g'

and eval_cl (cl : comm list) (fns : fn list) (s : sigma) (s_g : sigma) : value * sigma * sigma =
    match cl with
    | [] -> (Unit, s, s_g)
    | h::t -> (match h with
        | Return Some (e, _) -> let v, s_g = eval_exp e fns s s_g in v, s, s_g
        | Return None -> Unit, s, s_g
        | _ -> let s', s_g' = eval_comm h fns s s_g in
            eval_cl t fns s' s_g')

and eval_funct (((p, rt), cl) : fn) (fns : fn list) (s : sigma) (s_g : sigma) : value * sigma =
    let (v, s, s_g) = eval_cl cl fns s s_g in
    v, s_g

let rec default_value (t : etyp) =
    let init n v =
        let rec init_rec n v acc = if n = 0 then [] else (init_rec (n - 1) v (v::acc)) in
    init_rec n v [] in
    match t with
    | UnitTyp -> Unit
    | BoolTyp -> Bool false
    | IntTyp -> Num 0
    | FloatTyp -> Float 0.
    | VecTyp n -> VecLit (init n 0.)
    | MatTyp (m, n) -> MatLit (init m (init n 0.))
    | TransTyp (t1, t2) -> Unit
    | SamplerTyp n -> Unit
    | SamplerCubeTyp -> Unit
    | AbsTyp _ -> Unit
    | ArrTyp _ -> Unit
    
let start_eval (fns : fn list) (gv : global_vars) : unit =
    let add_arg = (fun acc (_, t, name, _) -> Assoc.update name (default_value t) acc) in
    let s_g = (List.fold_left (add_arg) Assoc.empty gv) in
    match fst (fn_lookup "main" fns) with
    | None -> failwith "Typechecker failed to find lack of main"
    | Some main -> eval_funct main fns Assoc.empty s_g |> ignore

let eval_prog (p : prog) (gv : global_vars) : unit =
    start_eval p gv