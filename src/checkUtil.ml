open Util
open GatorAst
open GatorAstPrinter
open Contexts

exception TypeException of string
exception TypeExceptionMeta of string * metadata
exception DimensionException of int * int

(* Produces an empty set of gator contexts with a starting metadata *)
let init meta = let b = 
  {t=Assoc.empty; g=Assoc.empty; d=Assoc.empty; 
  c=Assoc.empty; o=Assoc.empty; l=Assoc.empty} in
  {m=Assoc.empty; p=Assoc.empty; ps=Assoc.empty; pm=Assoc.empty; meta=meta; _bindings=b }

let with_m cx m' = {cx with m=m'}
let with_p cx p' = {cx with p=p'}
let with_ps cx ps' = {cx with ps=ps'}
let with_pm cx pm' = {cx with pm=pm'}
let with_meta cx meta' = {cx with meta=meta'}

let get_m cx x = if Assoc.mem x cx.m then Assoc.lookup x cx.m else 
  raise (TypeExceptionMeta ("Undefined modifiable item " ^ x, cx.meta))
let get_p cx x = if Assoc.mem x cx.p then Assoc.lookup x cx.p else 
  raise (TypeExceptionMeta ("Undefined function " ^ x, cx.meta))
let get_ps cx x = if Assoc.mem x cx.ps then Assoc.lookup x cx.ps else 
  raise (TypeExceptionMeta ("Undefined canonical item " ^ x, cx.meta))
let get_pm cx x = if Assoc.mem x cx.pm then Assoc.lookup x cx.pm else 
  raise (TypeExceptionMeta (x ^ " not found in parameterization " ^ string_of_parameterization cx.pm, cx.meta))

(* Finds which context in which to find the given string *)
let find_safe cx x =
  if Assoc.mem x cx._bindings.l then match Assoc.lookup x cx._bindings.l with
  | LTau -> Some (Tau (Assoc.lookup x cx._bindings.t))
  | LGamma -> Some (Gamma (Assoc.lookup x cx._bindings.g))
  | LDelta -> Some (Delta (Assoc.lookup x cx._bindings.d))
  | LChi -> Some (Chi (Assoc.lookup x cx._bindings.c))
  | LOmega -> Some (Omega (Assoc.lookup x cx._bindings.o))
  else None

(* Binds a string with value to the correct lookup context *)
let bind (cx : contexts) (x : string) (b : binding) : contexts =
  if Assoc.mem x cx._bindings.l
  then raise (TypeExceptionMeta ("Duplicate use of the name " ^ x, cx.meta)) else 
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  match b with
  | Tau t' ->   update_bindings { _b with l=Assoc.update x LTau _b.l; t=Assoc.update x t' _b.t }
  | Gamma g' -> update_bindings { _b with l=Assoc.update x LTau _b.l; g=Assoc.update x g' _b.g }
  | Delta d' -> update_bindings { _b with l=Assoc.update x LTau _b.l; d=Assoc.update x d' _b.d }
  | Chi c' ->   update_bindings { _b with l=Assoc.update x LTau _b.l; c=Assoc.update x c' _b.c }
  | Omega o' -> update_bindings { _b with l=Assoc.update x LTau _b.l; o=Assoc.update x o' _b.o }

(* Clears the given lookup context of elements *)
let clear (cx : contexts) (b : binding_context) =
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let build_l l xs = Assoc.gen_context (List.fold_left (fun acc (x, v) -> 
    if List.mem x xs then acc else (x, v)::acc) [] l) in
  let clear c = build_l (Assoc.bindings _b.l) (List.map fst (Assoc.bindings c)) in
  match b with
  | LTau ->   update_bindings { _b with l=clear _b.t; t=Assoc.empty }
  | LGamma -> update_bindings { _b with l=clear _b.g; g=Assoc.empty }
  | LDelta -> update_bindings { _b with l=clear _b.d; g=Assoc.empty }
  | LChi ->   update_bindings { _b with l=clear _b.c; c=Assoc.empty }
  | LOmega -> update_bindings { _b with l=clear _b.o; o=Assoc.empty }

let typ_ignore (t : typ) : unit = ignore t
let constrain_ignore (c : constrain) : unit = ignore c
let string_of_fn_inv ((s, tl) : fn_inv) : string = 
  s ^ "<" ^ string_of_list string_of_typ tl ^ ">"
let line_number (meta : metadata) : string = 
  ("Line: " ^ string_of_int(meta.pos_lnum))
let string_of_tau (pm, t : tau) =
  string_of_parameterization pm ^ " " ^  string_of_typ t
let string_of_mu (ml : mu) =  
  string_of_mod_list ml
let string_of_gamma (g : gamma) =
  string_of_typ g
let string_of_delta (f : delta) =
  string_of_frame f
let string_of_chi (c : chi) =
  string_of_coordinate_element c
let string_of_omega (o : omega) =
  string_of_prototype_element o
let string_of_phi (p : phi) =
  string_of_fn_typ p
let string_of_psi (ps : psi) : string =
  string_of_list (fun (t, p) -> "(" ^ string_of_typ t ^ ", " ^ string_of_fn_inv p ^ ")") ps

let get_typ (cx : contexts) (x : string) : tau =
  match find_safe cx x with
  | Some Tau t -> t
  | _ -> raise (TypeExceptionMeta ("Undefined type " ^ x, cx.meta))

let get_var (cx : contexts) (x : string) : gamma =
  match find_safe cx x with
  | Some Gamma g -> g
  | _ -> raise (TypeExceptionMeta ("Undefined variable " ^ x, cx.meta))

let get_frame (cx : contexts) (x : string) : delta =
  match find_safe cx x with
  | Some Delta d -> d
  | _ -> raise (TypeExceptionMeta ("Undefined frame " ^ x, cx.meta))

let rec get_frame_top (cx : contexts) (x : string) : int =
  match get_frame cx x with
  | FrameDim s -> get_frame_top cx s
  | FrameNum n -> n

let get_scheme (cx : contexts) (x : string) : chi =
  match find_safe cx x with
  | Some Chi c -> c
  | _ -> raise (TypeExceptionMeta ("Undefined coordinate scheme " ^ x, cx.meta))

let get_proto (cx : contexts) (x : string) : omega =
  match find_safe cx x with
  | Some Omega o -> o
  | _ -> raise (TypeExceptionMeta ("Undefined object " ^ x, cx.meta))