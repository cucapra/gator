open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Glsl_ops
open Util

type ovalue = | CoreValue of value | ArrValue of ovalue list
type sigma = ovalue Assoc.context

let rec string_of_ovalue (o : ovalue) =
    match o with
    | CoreValue v -> string_of_value v
    | ArrValue v -> string_of_array string_of_ovalue v

let rec fn_lookup (name : id) (fns : fn list) : (fn option * id list) =
    match fns with
    | [] -> (None, [])
    | h::t -> match h with ((id, (p, _, _)), _) -> 
        (if name = id then (Some h, List.map snd p) else fn_lookup name t)

and eval_texp ((e, t) : texp) (fns : fn list) (s : sigma) : ovalue * sigma =
    eval_exp e t fns s
and eval_exp (e : exp) (t : etyp) (fns : fn list) (s : sigma) : ovalue * sigma =
    match e with
    | Val v -> CoreValue v, s
    | Var x -> (try Assoc.lookup x s with _ -> Assoc.lookup x s), s
    | Arr a ->
        let result = List.fold_left (fun (vl, s_g) (e, _) -> let (v, s_g) = eval_exp e fns s in vl@[v], s_g) [] a in
        arr_op (fun x -> x) (fun x -> x) result
    | Index _ -> failwith "unimplemented op"
    | FnInv (id, tl, args) -> let (fn, p) = fn_lookup id fns in
        let (arg_vs, s_g') = (List.fold_left (fun (vl, s_g) (e, _) -> let (v, s_g) = eval_exp e fns s s_g in vl@[v], s_g) ([], s_g) args) in
        match fn with
        | None -> eval_glsl_fn id arg_vs, s_g'
        | Some f -> (match f with ((_, (names, _, _)), _) ->
            let add_arg = (fun acc (_, name) v -> Assoc.update name v acc) in
            eval_funct f fns (List.fold_left2 add_arg Assoc.empty names arg_vs)) s_g'

and eval_comm (c : comm) (fns : fn list) (s : sigma) : sigma =
    match c with
    | Skip -> s
    | Print e -> let v, s' = eval_texp e fns s in
        print_string (string_of_ovalue v ^ "\n"); s'
    | Exp e -> snd (eval_texp e fns s)
    | Decl (_, x, e)
    | Assign (x, e) -> let v, s' = eval_texp e fns s in
        (try let _ = Assoc.lookup x s in Assoc.update x v s' with
        _ -> Assoc.update x v s')
    | AssignOp ((x, xt), op, e) -> failwith "unimplemented op"
    | If (((b, _), c1), el, c2) ->
        let check_if b s_g = (match (eval_exp b fns s s_g) with
        | Bool b', s_g' -> b', s'
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
    match t with
    | UnitTyp -> Unit
    | BoolTyp -> Bool false
    | IntTyp -> Num 0
    | FloatTyp -> Float 0.
    | ParTyp _ -> Unit
    | ArrTyp _ -> Unit
    | AnyTyp | GenTyp -> Unit
    
let start_eval (fns : fn list) (gv : global_vars) : unit =
    let add_arg = fun acc (_, t, name, _) -> Assoc.update name (default_value t) acc in
    let s = List.fold_left add_arg Assoc.empty gv in
    match fst (fn_lookup "main" fns) with
    | None -> failwith "Typechecker failed to find lack of main"
    | Some main -> eval_funct main fns Assoc.empty s |> ignore

let eval_prog (p : prog) (gv : global_vars) : unit =
    start_eval p gv