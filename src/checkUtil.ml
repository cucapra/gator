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
  {t=Assoc.empty; g=Assoc.empty; d=Assoc.empty; c=Assoc.empty; 
    p=Assoc.empty; el=Assoc.empty; tl=Assoc.empty } in
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

let within cx s = 
  match find_typ cx s with
  | Some Chi _
  | None -> {cx with member=Some s}
  | _ -> debug_fail cx ("Invalid use of member " ^ s ^ " (should be a coordinate or prototype)")

let get_dimtyp (cx : contexts) (mem : string) =
  match find_typ cx mem with
  | Some Chi (_,d) -> FrameTyp d
  | None -> AnyFrameTyp
  | _ -> debug_fail cx ("Invalid use of member " ^ mem ^ " (should be a coordinate or prototype)")

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

let print_cxt   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_tau   "\n" cx._bindings.t)
let print_cxg   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_gamma "\n" cx._bindings.g)
let print_cxd   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_delta "\n" cx._bindings.d)
let print_cxc   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_chi   "\n" cx._bindings.c)
let print_cxp   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_phi   "\n" cx._bindings.p)
let print_cxm   (cx : contexts) = print_endline (Assoc.to_string_sep string_of_mu    "\n" cx.m)
let print_cxps  (cx : contexts) = print_endline (Assoc.to_string_sep string_of_psi   "\n" cx.ps)
let print_cxpm  (cx : contexts) = print_endline (string_of_parameterization cx.pm)
let print_cxmem (cx : contexts) = print_endline (string_of_option_removed (fun x -> x) cx.member)

let print_bindings (cx : contexts) =
  print_endline "Bindings:";
  print_endline "tau:"     ; print_cxt cx;
  print_endline "gamma:"   ; print_cxg cx;
  print_endline "delta:"   ; print_cxd cx;
  print_endline "chi:"     ; print_cxc cx;
  print_endline "phi:"     ; print_cxp cx
let print_context (cx : contexts) = 
  print_endline "Current context:";
  print_string "member:\t"  ; print_cxmem cx;
  print_string "pm:\t"      ; print_cxpm cx;
  print_bindings cx;
  print_string "mu:\t"      ; print_cxm cx;
  print_string "psi:\t"     ; print_cxps cx

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
      | ParTyp (s,pml) -> (match find_typ cx (lift s) with 
        | Some _ -> print_endline "hi"; CoordTyp (m, ParTyp(s, pml)), 
          List.fold_right (fun x acc -> 
            match x with | ParTyp (s, []) -> Assoc.update s pmt acc
            | _ -> debug_fail cx ("Invalid frame typ " ^ string_of_typ x)) pml pm | None -> t,pm)
      | ArrTyp (t',_) -> replace t' pm
      | _ -> t,pm
    in
    let rt',pm' = replace rt pm in
    let pr',pm'' = List.fold_right (fun (t,x) (pr',pm'') -> let l,r = replace t pm'' in (l,x)::pr',r) 
      pr ([], pm') in
    id,(ml,rt',id,pm'',pr',meta)
  in
  if Assoc.mem id' _b.p
  then let p' = f'::Assoc.lookup id' _b.p in update_bindings { _b with p=Assoc.update id' p' _b.p }
  else bind cx id' (Phi [f'])

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

let get_coordinate (cx : contexts) (x : string) : chi =
  match find_typ cx x with
  | Some Chi c -> c
  | _ -> error cx ("Undefined coordinate scheme " ^ x)

let get_functions_safe (cx : contexts) (id : string) : phi =
  let get_fn x = match find_exp cx x with
    | Some Phi p -> p | _ -> []
  in
  get_fn id @
  match cx.member with
  | Some c -> get_fn (c ^ "." ^ id)
  | None -> []  

let get_functions (cx : contexts) (id : string) : phi = 
  match get_functions_safe cx id with
  | [] -> error cx ("No type definition for function " ^ id)
  | p -> p
  