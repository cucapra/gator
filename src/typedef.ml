open GatorAst
open Util

(* Mapping from strings (typedef aliases) to types *)
type gamma = typ Assoc.context

let typedef_type (cx : gamma) (t : typ) : typ =
  match t with
  | Alias id -> Assoc.lookup id cx
  | _ -> t

let typedef_modification (cx : gamma) (m : modification) : modification =
  match m with
  | With (t, strs, b) -> With(typedef_type cx t, strs, b)
  | _ -> m

let typedef_params (cx : gamma) (ps : params) : params =
  List.map (fun (ml, t, str) ->
    (List.map (fun m -> typedef_modification cx m) ml,
    typedef_type cx t, str)) ps

let typedef_function_type (cx : gamma)
        ((ml, rt, id, params, meta) : fn_typ) : fn_typ =
  (List.map (fun m -> typedef_modification cx m) ml,
    typedef_type cx rt, id, typedef_params cx params, meta)

let typedef_prototype_element (cx : gamma) (e : prototype_element) : prototype_element =
  match e with
  | ProtoObject (ml, id, t) -> 
      ProtoObject(
        List.map (fun m -> typedef_modification cx m) ml,
        id,
        match t with
        | Some typ -> Some (typedef_type cx typ)
        | None -> None)
  | ProtoFn ft -> ProtoFn (typedef_function_type cx ft)

let typedef_aprototype_element (cx : gamma)
        ((elem, meta) : aprototype_element) : aprototype_element =
  (typedef_prototype_element cx elem, meta)

let typedef_prototype (cx : gamma) ((id, elems) : prototype) : prototype =
  (id, List.map (fun e -> typedef_aprototype_element cx e) elems)

let rec typedef_exp (cx : gamma) (e : exp) : exp =
  match e with
  | Arr ael -> Arr (List.map (fun ae -> typedef_aexp cx ae) ael)
  | Index (ae1, ae2) -> Index(typedef_aexp cx ae1, typedef_aexp cx ae2)
  | As (ae, t) -> As(typedef_aexp cx ae, typedef_type cx t)
  | In (ae, t) -> In(typedef_aexp cx ae, typedef_type cx t)
  | FnInv (str, tl, args) -> FnInv(str,
    List.map (fun t -> typedef_type cx t) tl,
    List.map (fun ae -> typedef_aexp cx ae) args)
  | _ -> e

and typedef_aexp (cx : gamma) (e, meta : aexp) : aexp =
  (typedef_exp cx e, meta)

let rec typedef_if_block (cx : gamma)
      ((ae, acl) : if_block) : if_block =
  (typedef_aexp cx ae,
  List.map (fun c -> typedef_acomm cx c) acl) 

and typedef_comm (cx : gamma) (c : comm) : comm =
  debug_print ">> typedef_comm";
  match c with
  | Print ae -> Print (typedef_aexp cx ae)
  | Exp ae -> Exp (typedef_aexp cx ae)
  | Decl (ml, t, s, ae) -> Decl(
    List.map (fun m -> typedef_modification cx m) ml,
    typedef_type cx t, s, typedef_aexp cx ae)
  | Assign (ae1, ae2) -> Assign(
    typedef_aexp cx ae1, typedef_aexp cx ae2)
  | AssignOp (ae1, str, ae2) -> AssignOp(
    typedef_aexp cx ae1, str, typedef_aexp cx ae2)
  | If (ib, ibl, clo) -> If(
    typedef_if_block cx ib,
    List.map (fun i -> typedef_if_block cx i) ibl,
    match clo with
    | Some cl -> Some (List.map (fun ac -> typedef_acomm cx ac) cl)
    | None -> None)
  | For (ac1, ae, ac2, acl) -> For(
    typedef_acomm cx ac1, typedef_aexp cx ae, typedef_acomm cx ac2,
    List.map (fun ac -> typedef_acomm cx ac) acl)
  | Return aeo -> Return (match aeo with
    | Some ae -> Some (typedef_aexp cx ae)
    | None -> None)
  | _ -> c 

and typedef_acomm (cx : gamma) ((comm, meta) : acomm) : acomm =
  (typedef_comm cx comm, meta)

let typedef_fn (cx : gamma) ((t, acomms) : fn) : fn =
  debug_print ">> typedef_fn";
  (typedef_function_type cx t, List.map (fun c -> typedef_acomm cx c) acomms)

let typedef_coordinate_element (cx : gamma) (ce : coordinate_element) =
   match ce with
   | CoordObjectAssign (ml, id, t) -> CoordObjectAssign(
     List.map (fun m -> typedef_modification cx m) ml,
     id, typedef_type cx t)
   | CoordFn fn -> CoordFn(typedef_fn cx fn)

let typedef_acoordinate_element (cx : gamma)
    ((ce, meta) : acoordinate_element) : acoordinate_element =
  (typedef_coordinate_element cx ce, meta)

let typedef_coordinate (cx : gamma)
        ((ml, id, str, acel) : coordinate) : coordinate =
  (List.map (fun m -> typedef_modification cx m) ml,
  id, str,
  List.map (fun ace -> typedef_acoordinate_element cx ace) acel)

let typedef_global_var (cx : gamma)
    ((ml, sq, t, id, aeo) : global_var) : global_var =
  (List.map (fun m -> typedef_modification cx m) ml,
  sq, typedef_type cx t, id,
  match aeo with
  | Some ae -> Some (typedef_aexp cx ae)
  | None -> None)

let rec typedef_term (cx : gamma) (t : term) : gamma * term list =
  debug_print (GatorAstPrinter.string_of_term t);
  match t with
  | Typedef (t, id) -> (Assoc.update id t cx, [])
  | Typ (ml, id, t) -> (cx, [Typ(
      List.map (fun m -> typedef_modification cx m) ml,
      id, typedef_type cx t)])
  | Prototype p -> (cx, [Prototype (typedef_prototype cx p)])
  | Fn f -> (cx, [Fn (typedef_fn cx f)])
  | Coordinate c -> (cx, [Coordinate (typedef_coordinate cx c)])
  | GlobalVar gv -> (cx, [GlobalVar (typedef_global_var cx gv)])
  | _ -> (cx, [t])

and typedef_aterm (cx : gamma) ((t, meta) : aterm) : gamma * aterm list =
  let (cx', terms) = typedef_term cx t in
  (cx', List.map (fun term -> (term, meta)) terms)

let typedef_prog (tl : prog) : prog =
  let cx, prog' =
    List.fold_left
      (fun acc at ->
        let (cx', aterms) = typedef_aterm (fst acc) at in
        (cx', (snd acc) @ aterms))
      (Assoc.empty, []) tl
  in
  prog'
