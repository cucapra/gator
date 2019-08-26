open Util
open GatorAst
open GatorAstPrinter
open Contexts

exception TypeException of string

let line_number (meta : metadata) : string = 
  ("Line: " ^ string_of_int(meta.pos_lnum))
let error cx s = raise (TypeException(line_number cx.meta ^ " -- " ^ s))
let debug_fail (cx : contexts) (s : string) =
  failwith (line_number cx.meta ^ "\t" ^ s)

(* Produces an empty set of gator contexts with a starting metadata *)
let init meta = let b = 
  {t=Assoc.empty; g=Assoc.empty; d=Assoc.empty; c=Assoc.empty; p=Assoc.empty; l=Assoc.empty } in
  {m=Assoc.empty; ps=Assoc.empty; pm=Assoc.empty; member=None; meta=meta; _bindings=b }

let with_m cx m' = {cx with m=m'}
let with_ps cx ps' = {cx with ps=ps'}
let with_pm cx pm' = {cx with pm=pm'}
let with_meta cx meta' = {cx with meta=meta'}
let clear_member cx = {cx with member=None}

let get_m cx x = if Assoc.mem x cx.m then Assoc.lookup x cx.m else 
  error cx ("Undefined modifiable item " ^ x)
let get_ps cx x = if Assoc.mem x cx.ps then Assoc.lookup x cx.ps else 
  error cx ("Undefined canonical item " ^ x)
let get_pm cx x = if Assoc.mem x cx.pm then Assoc.lookup x cx.pm else 
  error cx (x ^ " not found in parameterization " ^ string_of_parameterization cx.pm)

(* Finds which context in which to find the given string *)
let find_safe cx x =
  if Assoc.mem x cx._bindings.l then match Assoc.lookup x cx._bindings.l with
  | CTau -> Some (Tau (Assoc.lookup x cx._bindings.t))
  | CGamma -> Some (Gamma (Assoc.lookup x cx._bindings.g))
  | CDelta -> Some (Delta (Assoc.lookup x cx._bindings.d))
  | CChi -> Some (Chi (Assoc.lookup x cx._bindings.c))
  | CPhi -> Some (Phi (Assoc.lookup x cx._bindings.p))
  else None

(* Binds a string with value to the correct lookup context *)
let bind (cx : contexts) (x : string) (b : binding) : contexts =
  if Assoc.mem x cx._bindings.l
  then error cx ("Duplicate use of the name " ^ x) else 
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  match b with
  | Tau t' ->   update_bindings { _b with l=Assoc.update x CTau _b.l; t=Assoc.update x t' _b.t }
  | Gamma g' -> update_bindings { _b with l=Assoc.update x CGamma _b.l; g=Assoc.update x g' _b.g }
  | Delta d' -> update_bindings { _b with l=Assoc.update x CDelta _b.l; d=Assoc.update x d' _b.d }
  | Chi c' ->   update_bindings { _b with l=Assoc.update x CChi _b.l; c=Assoc.update x c' _b.c }
  | Phi p' ->   update_bindings { _b with l=Assoc.update x CPhi _b.l; p=Assoc.update x p' _b.p }

(* Clears the given lookup context of elements *)
let clear (cx : contexts) (b : binding_context) : contexts =
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let build_l l xs = Assoc.create (List.fold_left (fun acc (x, v) -> 
    if List.mem x xs then acc else (x, v)::acc) [] l) in
  let clear c = build_l (Assoc.bindings _b.l) (List.map fst (Assoc.bindings c)) in
  match b with
  | CTau ->   update_bindings { _b with l=clear _b.t; t=Assoc.empty }
  | CGamma -> update_bindings { _b with l=clear _b.g; g=Assoc.empty }
  | CDelta -> update_bindings { _b with l=clear _b.d; d=Assoc.empty }
  | CChi ->   update_bindings { _b with l=clear _b.c; c=Assoc.empty }
  | CPhi ->   update_bindings { _b with l=clear _b.p; p=Assoc.empty }

let within cx s = 
  match find_safe cx s with
  | Some Chi _
  | None -> {cx with member=Some s}
  | _ -> debug_fail cx ("Invalid use of member " ^ s ^ " (should be a coordinate or prototype)")

let get_dimtyp (cx : contexts) (mem : string) =
  match find_safe cx mem with
  | Some Chi (_,d) -> FrameTyp d
  | None -> AnyFrameTyp
  | _ -> debug_fail cx ("Invalid use of member " ^ mem ^ " (should be a coordinate or prototype)")

(* Adds the function 'f' to the context *)
(* If we are in a declaration 'member', then also updates the declaration of the members of 'f' *)
let add_function (cx : contexts) (f : fn_typ) : contexts =
  debug_print (">> add_function " ^ string_of_fn_typ f);
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let ml,rt,id,pm,pr,meta = f in
  let id', f' = match cx.member with
  | None -> id,f
  | Some m -> 
    let lift s = m ^ "." ^ s in
    let pmt = get_dimtyp cx m in
    let rec replace t pm =
      match t with
      | ParTyp (s,pml) -> (match find_safe cx (lift s) with 
        | Some _ -> ParTyp (lift s,pml), 
          List.fold_left (fun acc x -> 
            match x with | ParTyp (s, []) -> Assoc.update s pmt acc
            | _ -> debug_fail cx ("Invalid frame typ " ^ string_of_typ x)) pm pml | None -> t,pm)
      | ArrTyp (t',_) -> replace t' pm
      | _ -> t,pm
    in
    let id' = lift id in
    let rt',pm' = replace rt pm in
    let pr',pm'' = List.fold_left (fun (pr',pm'') (t,x) -> let l,r = replace t pm'' in (l,x)::pr',r) 
      ([], pm') pr in
    id',(ml,rt',id',pm'',pr',meta)
  in
  if Assoc.mem id' _b.p
  then let p' = f'::Assoc.lookup id' _b.p in update_bindings { _b with p=Assoc.update id' p' _b.p }
  else bind cx id' (Phi [f'])

let rename_fn (f : string -> string) (a,b,id,c,d,e:fn_typ) : fn_typ = a,b,f id,c,d,e

let ignore_typ (t : typ) : unit = ignore t
let ignore_dexp (d : dexp) : unit = ignore d
let ignore_typ_context (t : typ Assoc.context) : unit = ignore t
let string_of_fn_inv ((s, tl) : fn_inv) : string = 
  s ^ "<" ^ string_of_list string_of_typ tl ^ ">"
let string_of_tau (pm, t : tau) =
  string_of_parameterization pm ^ " " ^  string_of_typ t
let string_of_mu (ml : mu) =  
  string_of_mod_list ml
let string_of_gamma (g : gamma) =
  string_of_typ g
let string_of_delta (f : delta) =
  string_of_dexp f
let string_of_chi (p,d : chi) =
  "implements " ^ p ^ " with dimension " ^ string_of_dexp d
let string_of_phi (p : phi) =
  string_of_list string_of_fn_typ (List.map (rename_fn (fun x -> "%")) p)
let string_of_psi (ps : psi) : string =
  string_of_list (fun (t, p) -> "(" ^ string_of_typ t ^ ", " ^ string_of_fn_inv p ^ ")") ps

let string_of_cxt   (cx : contexts) = Assoc.to_string_sep string_of_tau   "\n" cx._bindings.t
let string_of_cxg   (cx : contexts) = Assoc.to_string_sep string_of_gamma "\n" cx._bindings.g
let string_of_cxd   (cx : contexts) = Assoc.to_string_sep string_of_delta "\n" cx._bindings.d
let string_of_cxc   (cx : contexts) = Assoc.to_string_sep string_of_chi   "\n" cx._bindings.c
let string_of_cxp   (cx : contexts) = Assoc.to_string_sep string_of_phi   "\n" cx._bindings.p
let string_of_cxm   (cx : contexts) = Assoc.to_string_sep string_of_mu    "\n" cx.m
let string_of_cxps  (cx : contexts) = Assoc.to_string_sep string_of_psi   "\n" cx.ps
let string_of_cxpm  (cx : contexts) = string_of_parameterization cx.pm
let string_of_cxmem (cx : contexts) = string_of_option_removed (fun x -> x) cx.member

let string_of_bindings (cx : contexts) =
  "Bindings:"
  ^ "\ntau:\n"     ^ string_of_cxt cx
  ^ "\ngamma:\n"   ^ string_of_cxg cx
  ^ "\ndelta:\n"   ^ string_of_cxd cx
  ^ "\nchi:\n"     ^ string_of_cxc cx
  ^ "\nphi:\n"     ^ string_of_cxp cx
let string_of_context (cx : contexts) = 
  "Current context:" 
  ^ "\nmember:\t"  ^ string_of_cxmem cx 
  ^ "\npm:\t"      ^ string_of_cxpm cx
  ^ "\n" ^ string_of_bindings cx
  ^ "\nmu:\t"      ^ string_of_cxm cx
  ^ "\npsi:\t"     ^ string_of_cxps cx

let option_clean (x : 'a option) : 'a =
  match x with | Some x -> x | None -> failwith "Failed option assumption"

let get_typ_safe (cx : contexts) (id : string) : tau option = 
  match find_safe cx id with
  | Some Tau t -> Some t
  | _ -> None

let get_typ (cx : contexts) (id : string) : tau =
  match get_typ_safe cx id with
  | Some t -> t
  | _ -> error cx ("Undefined type " ^ id)

let get_var (cx : contexts) (x : string) : gamma =
  match find_safe cx x with
  | Some Gamma g -> g
  | _ -> error cx ("Undefined variable " ^ x)

let get_frame (cx : contexts) (x : string) : delta =
  match find_safe cx x with
  | Some Delta d -> d
  | _ -> error cx ("Undefined frame " ^ x)

let get_coordinate (cx : contexts) (x : string) : chi =
  match find_safe cx x with
  | Some Chi c -> c
  | _ -> error cx ("Undefined coordinate scheme " ^ x)

let get_functions_safe (cx : contexts) (id : string) : phi =
  let get_fn x = match find_safe cx x with
    | Some Phi p -> p | _ -> []
  in
  match cx.member with
  | Some c -> let res = get_fn (c ^ "."  ^ id) in 
    (match res with
    | [] -> get_fn id
    | _ -> res)
  | None -> get_fn id

let get_functions (cx : contexts) (id : string) : phi = 
  match get_functions_safe cx id with
  | [] -> error cx ("No type definition for function " ^ id)
  | p -> p
  