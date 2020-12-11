open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Glsl_ops
open Util

type ovalue = CoreValue of value | ArrValue of ovalue list
type sigma = ovalue Assoc.context

let rec string_of_ovalue (o : ovalue) =
  match o with
  | CoreValue v -> string_of_value v
  | ArrValue v -> string_of_array string_of_ovalue v

let unwrap_vec (o : ovalue) : float list =
  let fail _ = failwith ("Expected a vec, got " ^ string_of_ovalue o) in
  match o with
  | ArrValue a ->
      List.fold_right
        (fun x ->
          List.cons
            ( match x with
            | CoreValue (Float f) -> f
            | CoreValue (Num n) -> float_of_int n
            | _ -> fail () ))
        a []
  | _ -> fail ()

let unwrap_mat (o : ovalue) : float list list =
  let fail _ = failwith ("Expected a vec, got " ^ string_of_ovalue o) in
  match o with
  | ArrValue a -> List.fold_right (fun x -> List.cons (unwrap_vec x)) a []
  | _ -> fail ()

let wrap_vec (v : float list) : ovalue =
  ArrValue (List.fold_right (fun x -> List.cons (CoreValue (Float x))) v [])

let wrap_mat (m : float list list) : ovalue =
  ArrValue (List.fold_right (fun x -> List.cons (wrap_vec x)) m [])

let rec fn_lookup (name : id) (fns : fn list) : fn option * id list =
  match fns with
  | [] -> (None, [])
  | h :: t -> (
    match h with
    | (_, id, _, p), _ ->
        if name = id then (Some h, List.map tr_thd p) else fn_lookup name t )

and internal_fn (name : id) (args : ovalue list) : ovalue =
  let fail () =
    failwith
      ( "Invalid application of " ^ name ^ " with arguments "
      ^ string_of_list string_of_ovalue args ) in
  let as_unop () : ovalue = match args with [a] -> a | _ -> fail () in
  let as_binop () : ovalue * ovalue =
    match args with [a; b] -> (a, b) | _ -> fail () in
  let num_unop (int_op : int -> int) (float_op : float -> float) : ovalue =
    match as_unop () with
    | CoreValue (Num n) -> CoreValue (Num (int_op n))
    | CoreValue (Float f) -> CoreValue (Float (float_op f))
    | ArrValue a -> ArrValue (List.map (fun x -> internal_fn name [x]) a)
    | _ -> fail () in
  let num_binop (int_op : int -> int -> int)
      (float_op : float -> float -> float)
      (string_op : string -> string -> string) : ovalue =
    match as_binop () with
    | CoreValue (Num n1), CoreValue (Num n2) -> CoreValue (Num (int_op n1 n2))
    | CoreValue (Float f1), CoreValue (Float f2) ->
        CoreValue (Float (float_op f1 f2))
    | CoreValue (StringVal s1), CoreValue (StringVal s2) ->
        CoreValue (StringVal (string_op s1 s2))
    | ArrValue a1, ArrValue a2 ->
        ArrValue (List.map2 (fun x y -> internal_fn name [x; y]) a1 a2)
    | _ -> fail () in
  match name with
  | "+" -> num_binop ( + ) ( +. ) (fun s1 s2 -> String.concat "" [s1; s2])
  | "-" ->
      if List.length args == 1 then num_unop ( ~- ) ( ~-. )
      else if List.length args == 2 then
        num_binop ( - ) ( -. ) (fun _ -> failwith "unsupported")
      else fail ()
  | _ -> failwith ("Unsupported function " ^ name)

and eval_texp ((e, t) : texp) (fns : fn list) (s : sigma) : ovalue * sigma =
  eval_exp e t fns s

and eval_exp (e : exp) (t : etyp) (fns : fn list) (s : sigma) : ovalue * sigma =
  match e with
  | Val v -> (CoreValue v, s)
  | Var x -> ((try Assoc.lookup x s with _ -> Assoc.lookup x s), s)
  | Arr a ->
      let result, s' =
        List.fold_left
          (fun (vl, s_g) e ->
            let v, s = eval_texp e fns s in
            (vl @ [v], s))
          ([], s) a in
      (ArrValue result, s')
  | Index _ -> failwith "unimplemented op"
  | FnInv (id, tl, args) -> (
      let fn, p = fn_lookup id fns in
      let arg_vs, s' =
        List.fold_left
          (fun (vl, s) e ->
            let v, s' = eval_texp e fns s in
            (vl @ [v], s))
          ([], s) args in
      match fn with
      | None -> (internal_fn id arg_vs, s')
      | Some f -> (
        match f with
        | (_, _, _, names), _ ->
            let add_arg acc (_, _, name) v = Assoc.update name v acc in
            eval_funct f fns (List.fold_left2 add_arg Assoc.empty names arg_vs)
        ))
  | FieldSelect (_, _) -> failwith "unimplemented op"

and eval_comm (c : comm) (fns : fn list) (s : sigma) : sigma =
  match c with
  | Skip -> s
  | Print e ->
      let v, s' = eval_texp e fns s in
      print_string (string_of_ovalue v ^ "\n") ;
      s'
  | Exp e -> snd (eval_texp e fns s)
  | Decl (_, x, e) -> failwith "unimplemented op"
  | Assign (x, e) -> failwith "unimplemented op"
  | AssignOp (x, op, e) -> failwith "unimplemented op"
  | If ((b, c1), el, c2) -> (
      let check_if b s =
        match eval_texp b fns s with
        | CoreValue (Bool b'), s' -> (b', s')
        | _ ->
            failwith
              "Typechecker failure: expected a boolean in 'if' expression" in
      let check_cmd b c acc =
        match acc with
        | Some s -> acc
        | None ->
            let v, s' = check_if b s in
            if v then
              let _, s' = eval_cl c fns s in
              Some s'
            else None in
      match
        List.fold_left
          (fun acc (b, c) -> check_cmd b c acc)
          (check_cmd b c1 None) el
      with
      | Some s' -> s'
      | None -> (
        match c2 with
        | Some c ->
            let _, s' = eval_cl c fns s in
            s'
        | None -> s ) )
  | For (c1, b, c2, cl) ->
      let rec loop s =
        match eval_texp b fns s with
        | CoreValue (Bool br), s_g ->
            if br then
              let _, s' = eval_cl cl fns s in
              loop (eval_comm c2 fns s')
            else s
        | _ -> failwith "Typechecker failure -- expected a boolean expression"
      in
      loop (eval_comm c1 fns s)
  | Return e -> s
  | ExactCodeComm ec -> s

and eval_cl (cl : comm list) (fns : fn list) (s : sigma) : ovalue * sigma =
  match cl with
  | [] -> (CoreValue Unit, s)
  | h :: t -> (
    match h with
    | Return (Some e) ->
        let v, s_g = eval_texp e fns s in
        (v, s)
    | Return None -> (CoreValue Unit, s)
    | _ ->
        let s' = eval_comm h fns s in
        eval_cl t fns s' )

and eval_funct (((rt, _, _, _), cl) : fn) (fns : fn list) (s : sigma) :
    ovalue * sigma =
  eval_cl cl fns s

let rec default_value (t : etyp) : ovalue =
  match t with
  | UnitTyp -> CoreValue Unit
  | BoolTyp -> CoreValue (Bool false)
  | IntTyp -> CoreValue (Num 0)
  | FloatTyp -> CoreValue (Float 0.)
  | StringTyp -> CoreValue (StringVal "")
  | ParTyp _ -> CoreValue Unit
  | ArrTyp (t', d) -> CoreValue Unit
  | AnyTyp | GenTyp -> CoreValue Unit
  | ExactCodeTyp -> CoreValue Unit
  | StructureTyp -> CoreValue Unit

let start_eval (fns : term list) : unit = failwith "unimplemented"

(* let add_arg = fun acc (_, t, name, _) -> Assoc.update name (default_value t) acc in
   let s = List.fold_left add_arg Assoc.empty gv in
   match fst (fn_lookup "main" fns) with
   | None -> failwith "Typechecker failed to find lack of main"
   | Some main -> eval_funct main fns s |> ignore *)

let eval_prog (p : prog) : unit = start_eval p
