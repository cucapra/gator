open Util
open GatorAst
open GatorAstPrinter
open CheckContexts

exception TypeException of string

let rename_fn (f : string -> string) (a,b,id,c,d:fn_typ) : fn_typ = a,b,f id,c,d

let ignore_typ (t : typ) : unit = ignore t
let ignore_dexp (d : dexp) : unit = ignore d
let ignore_typ_context (t : typ Assoc.context) : unit = ignore t
let string_of_fn_inv ((s, tl) : fn_inv) : string = 
  s ^ "<" ^ string_of_list string_of_typ tl ^ ">"
let string_of_tau (ml, pm, t : tau) =
  string_of_mod_list ml ^ string_of_parameterization pm ^ " " ^  string_of_typ t
let string_of_gamma (g : gamma) =
  string_of_typ g
let string_of_delta (f : delta) =
  string_of_dexp f
let string_of_chi (pm, c : chi) =
  string_of_option_removed (fun p -> "implements " ^ p) c ^ string_of_parameterization pm
let string_of_phi (p : phi) =
  string_of_list (fun (c, f) -> string_of_option_removed (fun x -> x ^ "->") c ^ string_of_fn_typ f) (List.map 
    (fun (c, f) -> c, rename_fn (fun x -> "%" ^ let cut = if String.contains x '_' then String.rindex x '_' else 0 in
      String.sub x cut (String.length x - cut)) f) p)
let string_of_psi (ps : psi) : string =
  string_of_list (fun x -> x) ps

let print_cxt   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_tau   "\n" cx._bindings.t)
let print_cxg   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_gamma "\n" cx._bindings.g)
let print_cxd   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_delta "\n" cx._bindings.d)
let print_cxc   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_chi   "\n" cx._bindings.c)
let print_cxp   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_phi   "\n" cx._bindings.p)
let print_cxps  (cx : contexts) = print_endline (Assoc.to_string_sep string_of_psi   "\n" cx.ps)
let print_cxpm  (cx : contexts) = print_endline (string_of_parameterization cx.pm)

let print_bindings (cx : contexts) =
  print_endline "Bindings:";
  print_endline "tau:"     ; print_cxt cx;
  print_endline "gamma:"   ; print_cxg cx;
  print_endline "delta:"   ; print_cxd cx;
  print_endline "chi:"     ; print_cxc cx;
  print_endline "phi:"     ; print_cxp cx
let print_context (cx : contexts) = 
  print_endline "Current context:";
  print_string "pm:\t"  ; print_cxpm cx; print_endline "";
  print_bindings cx;
  print_string "psi:\t" ; print_cxps cx

let line_number (meta : metadata) : string = 
  ("Line: " ^ string_of_int(meta.pos_lnum))
let error_meta meta s = raise (TypeException(line_number meta ^ " -- " ^ s))
let error cx s = 
  if !debug then print_context cx;
  error_meta cx.meta s
let debug_fail (cx : contexts) (s : string) =
  if !debug then print_context cx;
  failwith (line_number cx.meta ^ "\t" ^ s)

let with_extern cx externs' = {cx with externs=externs'}
let add_prog cx x p = if Assoc.mem x cx.externs
  then error cx ("Duplicate use of external filename " ^ x)
  else with_extern cx (Assoc.update x p cx.externs)

(* Produces an empty set of gator contexts with a starting metadata and external programs *)
let init meta progs = 
  let b = {t=Assoc.empty; g=Assoc.empty; d=Assoc.empty; c=Assoc.empty; 
    p=Assoc.empty; el=Assoc.empty; tl=Assoc.empty } in
  let cx = {ps=Assoc.empty; pm=Assoc.empty; scheme_pm=Assoc.empty; 
    externs=Assoc.empty; meta=meta; _bindings=b } in
  List.fold_left (fun acc (x, p) -> add_prog acc x p) cx (Assoc.bindings progs)

let with_ps cx ps' = {cx with ps=ps'}
let with_pm cx pm' = {cx with pm=pm'}
let with_scheme cx pm' = {cx with scheme_pm=pm'}
let with_meta cx meta' = {cx with meta=meta'}

let get_ps cx x = if Assoc.mem x cx.ps then Assoc.lookup x cx.ps else 
  error cx ("Undefined canonical item " ^ x)
let get_pm cx x = if Assoc.mem x cx.pm then Assoc.lookup x cx.pm else 
  error cx (x ^ " not found in parameterization " ^ string_of_parameterization cx.pm)
let get_prog cx x = if Assoc.mem x cx.externs then Assoc.lookup x cx.externs else 
  debug_fail cx (x ^ " not found in the list of imported files")

(* Finds which context in which to find the given string *)
let find_exp cx x =
  if Assoc.mem x cx._bindings.el then match Assoc.lookup x cx._bindings.el with
  | CGamma -> Some (Gamma (Assoc.lookup x cx._bindings.g))
  | CPhi -> Some (Phi (Assoc.lookup x cx._bindings.p))
  else None

let find_typ cx x =
  if Assoc.mem x cx._bindings.tl then match Assoc.lookup x cx._bindings.tl with
  | CTau -> Some (Tau (Assoc.lookup x cx._bindings.t))
  | CDelta -> Some (Delta (Assoc.lookup x cx._bindings.d))
  | CChi -> Some (Chi (Assoc.lookup x cx._bindings.c))
  else None

(* Binds a string with value to the correct lookup context *)
let bind (cx : contexts) (x : string) (b : binding) : contexts =
  let fail _ = error cx ("Duplicate use of the name " ^ x) in
  let ce _ = if Assoc.mem x cx._bindings.el then fail () in
  let ct _ = if Assoc.mem x cx._bindings.tl then fail () in
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  match b with
  | Tau t' ->   ct(); update_bindings { _b with tl=Assoc.update x CTau _b.tl; t=Assoc.update x t' _b.t }
  | Gamma g' -> ce(); update_bindings { _b with el=Assoc.update x CGamma _b.el; g=Assoc.update x g' _b.g }
  | Delta d' -> ct(); update_bindings { _b with tl=Assoc.update x CDelta _b.tl; d=Assoc.update x d' _b.d }
  | Chi c' ->   ct(); update_bindings { _b with tl=Assoc.update x CChi _b.tl; c=Assoc.update x c' _b.c }
  | Phi p' ->   ce(); update_bindings { _b with el=Assoc.update x CPhi _b.el; p=Assoc.update x p' _b.p }

(* Clears the given lookup context of elements *)
let clear (cx : contexts) (b : exp_bindings) : contexts =
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let build_l l xs = Assoc.create (List.fold_left (fun acc (x, v) -> 
    if List.mem x xs then acc else (x, v)::acc) [] l) in
  let clear c = build_l (Assoc.bindings _b.el) (List.map fst (Assoc.bindings c)) in
  match b with
  | CGamma -> update_bindings { _b with el=clear _b.g; g=Assoc.empty }
  | CPhi ->   update_bindings { _b with el=clear _b.p; p=Assoc.empty }

(* Resets the contexts cx to the state provided by the reference contexts cx_ref *)
let reset (cx : contexts) (cx_ref : contexts) (b : exp_bindings) : contexts =
  match b with
  | CGamma -> List.fold_left 
    (fun acc (x, g) -> bind acc x (Gamma g)) (clear cx b) (Assoc.bindings cx_ref._bindings.g)
  | CPhi ->   List.fold_left 
    (fun acc (x, p) -> bind acc x (Phi p)) (clear cx b) (Assoc.bindings cx_ref._bindings.p)

let has_modification (cx : contexts) (ml : modification list) (m : modification) : bool =
  List.fold_right (fun mc acc -> mc = m || acc) ml false 

(* Returns an unordered list of all types with the given modification *)
let get_with_modification (cx : contexts) (m : modification) : string list =
  List.fold_right (fun (s, (ml, _, _)) acc -> 
    if has_modification cx ml m then s::acc else acc)
    (Assoc.bindings cx._bindings.t) ["start"]

let get_ml_pm (cx : contexts) (ml : modification list) : parameterization =
  let get_ml_pm_rec (pm : parameterization) (m : modification) =
    match m with
    | With (t, sl) -> 
      let fail s = error cx ("Duplicate parameterization assignments to variable " ^ s) in
      List.fold_right (fun s acc -> if Assoc.mem s acc then fail s
        else Assoc.update s t acc) sl pm
    | _ -> pm
  in
  List.fold_left get_ml_pm_rec (Assoc.empty) ml

let option_clean (x : 'a option) : 'a =
  match x with | Some x -> x | None -> failwith "Failed option assumption"

let get_typ_safe (cx : contexts) (id : string) : tau option = 
  match find_typ cx id with
  | Some Tau t -> Some t
  | _ -> None

let get_typ (cx : contexts) (id : string) : tau =
  match get_typ_safe cx id with
  | Some t -> t
  | _ -> error cx ("Undefined type " ^ id)

let get_var (cx : contexts) (x : string) : gamma =
  match find_exp cx x with
  | Some Gamma g -> g
  | _ -> error cx ("Undefined variable " ^ x)

let get_frame (cx : contexts) (x : string) : delta =
  match find_typ cx x with
  | Some Delta d -> d
  | _ -> error cx ("Undefined frame " ^ x)

let get_scheme (cx : contexts) (x : string) : chi =
  match find_typ cx x with
  | Some Chi c -> c
  | _ -> error cx ("Undefined coordinate scheme " ^ x)

let get_functions_safe (cx : contexts) (id : string) : phi =
  let get_fn x = match find_exp cx x with
    | Some Phi p -> p | _ -> []
  in
  get_fn id

let get_functions (cx : contexts) (id : string) : phi = 
  match get_functions_safe cx id with
  | [] -> error cx ("No type definition for function " ^ id)
  | p -> p

(* Adds the function 'f' to the context *)
(* Requires information on the declaraing class name if applicable *)
(* Note that we do not update the string, as with types, to help with the search algorithm *)
let bind_function (cx : contexts) (f : fn_typ) (scheme : string option) : string * contexts =
  debug_print (">> bind_function " ^ string_of_fn_typ f);
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let _,_,id,_,_ = f in
  (* Rename each function except main for overloading purposes -- this gives correct invocation behavior when rewriting *)
  if Assoc.mem id _b.p
  then
    let fnl = Assoc.lookup id _b.p in
    let f_write = rename_fn (fun x -> x ^ "_" ^ string_of_int (List.length fnl)) f in
    let _,_,id_write,_,_ = f_write in
    let p' = (scheme, f_write)::fnl in
    id_write, update_bindings { _b with p=Assoc.update id p' _b.p }
  else 
    let f_write = if id = "main" then f else rename_fn (fun x -> x ^ "_0") f in
    let _,_,id_write,_,_ = f_write in
    id_write, bind cx id (Phi [scheme, f_write])

let rec map_aexp (cx : contexts) (fs : string -> string) (f : typ -> typ) (ae : aexp) : aexp =
  debug_print (">> map_aexp " ^ string_of_aexp ae);
  let mt = map_aexp cx fs f in
  let e,meta = ae in
  match e with 
  | Arr l -> Arr (List.map mt l), meta
  | Index (e1, e2) -> Index (mt e1, mt e2),meta
  | As (e1, t) -> As (mt e1, f t), meta
  | In (e1, t) -> In (mt e1, f t), meta
  | FnInv(s, tl, ael) -> FnInv(fs s, List.map f tl, List.map mt ael),meta
  | _ -> ae

let map_mod (cx : contexts) (f : typ -> typ) (m : modification) : modification =
  debug_print (">> map_mod " ^ string_of_modification m);
  match m with
  | With(t, b) -> With(f t, b)
  | _ -> m

let rec map_acomm (cx : contexts) (fs : string -> string) (fe : exp -> exp) (ft : typ -> typ) (ac : acomm) 
  : acomm =
  debug_print (">> map_acomm " ^ string_of_acomm ac);
  let c,meta = ac in
  let map_if_block (cx : contexts) (fe : exp -> exp) (ft : typ -> typ) (e, c : aexp * acomm list) 
    : aexp * acomm list =
    map_aexp cx fs ft e, List.map (map_acomm cx fs fe ft) c 
  in
  let et = map_aexp cx fs ft in
  let it = map_if_block cx fe ft in
  let ct = map_acomm cx fs fe ft in
  match c with
  | Skip -> ac
  | Print e -> Print(et e),meta
  | Exp e -> Exp(et e),meta
  | Decl (ml, t, s, e) -> Decl(ml, ft t, s, et e),meta
  | Assign (s, e) -> Assign(fs s, et e),meta
  | AssignOp (s1, s2, e) -> AssignOp(fs s1, fs s2, et e),meta
  | If (i, il, clo) -> If (it i, List.map it il, option_map (List.map ct) clo),meta
  | For (c1, e, c2, cl) -> For (ct c1, et e, ct c2, List.map ct cl),meta
  | Return e -> Return(option_map et e),meta

let map_fn_typ (cx : contexts) (fs : string -> string) (fe : exp -> exp) (ft : typ -> typ) (fn : fn_typ) 
  : fn_typ =
  debug_print (">> map_fn_typ " ^ string_of_fn_typ fn);
  let ml,rt,id,pr,m = fn in
  List.map (map_mod cx ft) ml,ft rt,id,List.map (fun (t,s)->(ft t,s)) pr,m

let map_fn (cx : contexts) (fs : string -> string) (fe : exp -> exp) (ft : typ -> typ) (fnt,cl : fn) : fn =
  debug_print (">> map_fn " ^ string_of_fn (fnt, cl));
  map_fn_typ cx fs fe ft fnt,List.map (map_acomm cx fs fe ft) cl

let map_aprototype_element (cx : contexts) (fs : string -> string) (fe : exp -> exp) (ft : typ -> typ) 
  (ap : aprototype_element)  : aprototype_element =
  let p, meta = ap in
  debug_print (">> map_aprototype_element " ^ string_of_prototype_element p);
  match p with
  | ProtoObject(ml, s, t) -> ProtoObject(List.map (map_mod cx ft) ml, s, option_map ft t), meta
  | ProtoFn(fn) -> ProtoFn(map_fn_typ cx fs fe ft fn), meta

let map_acoordinate_element (cx : contexts) (fs : string -> string) (fe : exp -> exp) (ft : typ -> typ) 
  (ac : acoordinate_element) : acoordinate_element =
  let c, meta = ac in
  debug_print (">> map_acoordinate_element " ^ string_of_coordinate_element c);
  match c with
  | CoordObjectAssign (ml, s, t) -> CoordObjectAssign(List.map (map_mod cx ft) ml, s, ft t), meta
  | CoordFn (fn) -> CoordFn(map_fn cx fs fe ft fn), meta
