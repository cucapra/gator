open CoreAst
open GatorAst
open GatorAstPrinter
open Util
open Printf
open Str
open CheckUtil
open CheckContexts
open Glsl_ops

(* The set of types that can't be written down shouldn't be
 * inferred by things like 'auto' *)
let is_illegal_typ (cx : contexts) (t : typ) : bool =
  match t with AnyTyp | AnyFrameTyp | FrameTyp _ -> true | _ -> false

let rec reduce_dexp (cx : contexts) (d : dexp) : int =
  debug_print (">> reduce_dexp " ^ string_of_dexp d) ;
  match d with
  | DimNum n -> n
  | DimVar x -> reduce_dexp cx (get_frame cx x)
  | DimPlus (l, r) -> reduce_dexp cx l + reduce_dexp cx r

let rec unwrap_abstyp (cx : contexts) (s : string) : typ =
  debug_print ">> unwrap_abstyp" ;
  match fst (get_pm cx s) with
  | ParTyp (s, tl) -> debug_fail cx "unimplemented partyp unwrapping"
  | p -> p

(* Replace all instances of the types in the given context,
 * returning true if something changed *)
let rec replace_abstype_query (c : typ Assoc.context) (t : typ) : typ * bool =
  debug_print (">> replace_abstype " ^ string_of_typ t) ;
  let is_abs s = Assoc.mem s c in
  match t with
  | ParTyp (s, tl) ->
      if is_abs s then (Assoc.lookup s c, true)
      else
        let tl', b =
          List.fold_right
            (fun t (acc, b) ->
              let t', b' = replace_abstype_query c t in
              (t' :: acc, b || b'))
            tl ([], false) in
        (ParTyp (s, tl'), b)
  | MemberTyp (t1, t2) ->
      let t1', b1 = replace_abstype_query c t1 in
      let t2', b2 = replace_abstype_query c t2 in
      (MemberTyp (t1', t2'), b1 || b2)
  | ArrTyp (t', d) ->
      let rt, b = replace_abstype_query c t' in
      (ArrTyp (rt, d), b)
  | _ -> (t, false)

let replace_abstype (c : typ Assoc.context) (t : typ) : typ =
  fst (replace_abstype_query c t)

let rec is_typ_eq (cx : contexts) (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  | UnitTyp, UnitTyp
   |BoolTyp, BoolTyp
   |IntTyp, IntTyp
   |FloatTyp, FloatTyp
   |StringTyp, StringTyp ->
      true
  | Literal t1, Literal t2 -> is_typ_eq cx t1 t2
  | ArrTyp (t1, d1), ArrTyp (t2, d2) ->
      is_typ_eq cx t1 t2 && reduce_dexp cx d1 = reduce_dexp cx d2
  | MemberTyp (t1, t2), MemberTyp (t3, t4) ->
      is_typ_eq cx t1 t3 && is_typ_eq cx t2 t4
  | ParTyp (s1, tl1), ParTyp (s2, tl2) ->
      s1 = s2
      &&
      if List.length tl1 = List.length tl2 then list_typ_eq cx tl1 tl2
      else false
  | _ -> false

and list_typ_eq (cx : contexts) (tl1 : typ list) (tl2 : typ list) : bool =
  List.fold_left2 (fun acc x y -> acc && is_typ_eq cx x y) true tl1 tl2

let delta_lookup (cx : contexts) (x : string) : typ =
  match get_frame cx x with DimVar v -> ParTyp (v, []) | d -> FrameTyp d

let rec is_subtype (cx : contexts) (to_check : typ) (target : typ) : bool =
  debug_print
    ( ">> is_subtype "
    ^ string_of_pair (string_of_typ to_check) (string_of_typ target) ) ;
  if is_typ_eq cx to_check target then true
  else
    match (to_check, target) with
    | BotTyp, _ -> true
    | _, BotTyp -> false
    | _, AnyTyp -> true
    | AnyTyp, _ -> false
    | BoolTyp, GenTyp -> false
    | _, GenTyp -> true
    | AnyFrameTyp, AnyFrameTyp -> true
    | FrameTyp _, AnyFrameTyp -> true
    | GenArrTyp t1, GenArrTyp t2 -> is_subtype cx t1 t2
    | ArrTyp (t, _), GenArrTyp c -> is_subtype cx t c
    | ArrTyp (t1, d1), ArrTyp (t2, d2) ->
        reduce_dexp cx d1 = reduce_dexp cx d2 && is_subtype cx t1 t2
    | ParTyp (s1, tl1), ParTyp (s2, tl2) ->
        s1 = s2
        && List.length tl1 = List.length tl2
        && is_subtype_list cx tl1 tl2
        || is_subtype cx (typ_step cx to_check) target
    | MemberTyp (t1, t2), MemberTyp (t3, t4) ->
        (* Note that we use is_subtype cx t1 t3 to account for potential parameterization *)
        (is_typ_eq cx t2 t4 && is_subtype cx t1 t3)
        || is_subtype cx (typ_step cx to_check) target
    | FrameTyp d1, FrameTyp d2 -> reduce_dexp cx d1 = reduce_dexp cx d1
    (* Type lookup cases *)
    (* Note that the primitive of a given type is the top representation of that type *)
    | Literal t, _ -> is_subtype cx target (primitive_deep cx to_check)
    | ParTyp (s, tl), _ -> is_subtype cx (typ_step cx to_check) target
    | MemberTyp (c, o), _ -> is_subtype cx (typ_step cx to_check) target
    | FrameTyp _, _ -> is_subtype cx (typ_step cx to_check) target
    | _ -> false

and is_subtype_list (cx : contexts) (l1 : typ list) (l2 : typ list) : bool =
  debug_print ">> is_subtype_list" ;
  if List.length l1 != List.length l2 then false
  else List.for_all2 (is_subtype cx) l1 l2

(* Given a parameterization and a list of types being invoked on that parameterization *)
(* Returns the appropriate concretized context if one exists *)
and match_parameterization_safe (cx : contexts) (pml : typ list) :
    typ Assoc.context option =
  debug_print
    (">> match_parameterization <" ^ string_of_list string_of_typ pml ^ ">") ;
  let pmb = Assoc.bindings cx.pm in
  if
    List.length pmb == List.length pml
    && List.fold_left2
         (fun acc (s, (t1, b)) t ->
           is_subtype cx t t1 && (not (is_typ_eq cx t t1 && b)) && acc)
         true pmb pml
  then
    Some
      (List.fold_left2
         (fun tcacc (s, _) t -> Assoc.update s t tcacc)
         Assoc.empty (Assoc.bindings cx.pm) pml)
  else None

(* Throws an error if there is no concretized context that exists *)
and match_parameterization (cx : contexts) (pml : typ list) : typ Assoc.context
    =
  match match_parameterization_safe cx pml with
  | Some r -> r
  | None ->
      error cx
        ( "Invalid parameterization provided by <"
        ^ string_of_list string_of_typ pml
        ^ ">" )

(* Looks up an object 'o' definition from a coordinate scheme 'c' *)
and chi_object_lookup (cx : contexts) (c : typ) (o : typ) : typ =
  let mstr = string_of_typ (MemberTyp (c, o)) in
  debug_print (">> chi_object_lookup " ^ mstr) ;
  let cn, f1, on, f2 =
    match (c, o) with
    | ParTyp (c, f1), ParTyp (o, f2) -> (c, f1, o, f2)
    | _ ->
        error cx
          ( "Invalid geometric type " ^ string_of_typ c ^ "." ^ string_of_typ o
          ^ "(Note that all geometric types must be of the form \
             scheme<frame>.object)" ) in
  (* For now, we just check that the parameterization on c is valid;
   * it doesn't mean anything when looking up the supertype *)
  let stc = match_parameterization (with_pm cx (fst (get_scheme cx cn))) f1 in
  let _, pmd, t = get_typ cx (cn ^ "." ^ on) in
  let tc = match_parameterization (with_pm cx pmd) f2 in
  replace_abstype stc (replace_abstype tc t)

(* Looks up a supertype of the given partyp *)
and tau_lookup (cx : contexts) (x : id) (pml : typ list) : typ =
  (* If the given type evaluates to a declared tag, return it *)
  (* If the return type would be a top type, resolve the dimension to a number *)
  debug_print
    (">> tau_lookup " ^ x ^ "<" ^ string_of_list string_of_typ pml ^ ">") ;
  let _, pmd, t = get_typ cx x in
  let tc = match_parameterization (with_pm cx pmd) pml in
  replace_abstype tc t

(* Steps types up a level in the subtyping tree *)
(* Returns None if given a primitive type,
 * illegal geometric type, or external type (they have no supertype) *)
and typ_step (cx : contexts) (t : typ) : typ =
  debug_print (">> typ_step " ^ string_of_typ t) ;
  let t', modified =
    replace_abstype_query (Assoc.map fst (Assoc.union cx.scheme_pm cx.pm)) t
  in
  if modified then t'
  else
    match t with
    | ParTyp (s, tl) -> (
      (* Looks up the supertype of s --
       * note that the behavior differs when inside a definition, defined by 'fail' *)
      match find_typ cx s with
      | Some (Tau _) -> tau_lookup cx s tl
      | Some (Delta _) -> delta_lookup cx s
      | Some (Chi (_, s)) -> (
        match s with Some p -> ParTyp (p, []) | None -> AnyTyp )
      | _ -> error cx ("Unknown type " ^ string_of_typ t) )
    | MemberTyp (c, o) -> chi_object_lookup cx c o
    (* Note that literals are always the first thing to be unwrapped *)
    | Literal t' -> t'
    | ArrTyp (t', d) -> (
      match t' with AnyTyp -> AnyTyp | _ -> ArrTyp (typ_step cx t', d) )
    | _ -> AnyTyp

(* Produces the primitive of the given type (non-declared non-literal) *)
and primitive_deep (cx : contexts) (t : typ) : typ =
  debug_print (">> primitive " ^ string_of_typ t) ;
  let rec is_primitive t' : bool =
    match t' with
    | ParTyp _ | MemberTyp _ | Literal _ -> false
    | ArrTyp (t'', _) | GenArrTyp t'' -> is_primitive t''
    | _ -> true in
  if is_primitive t then t else primitive_deep cx (typ_step cx t)

let rec primitive (cx : contexts) (t : typ) : typ =
  debug_print (">> primitive " ^ string_of_typ t) ;
  let rec is_primitive t' : bool =
    match t' with ParTyp _ | MemberTyp _ | Literal _ -> false | _ -> true in
  if is_primitive t then t else primitive cx (typ_step cx t)

let rec greatest_common_child (cx : contexts) (t1 : typ) (t2 : typ) : typ =
  debug_print ">> greatest_common_child" ;
  if is_subtype cx t1 t2 then t1
  else if is_subtype cx t2 t1 then t2
  else
    let top = primitive_deep cx t1 in
    if is_typ_eq cx top (primitive_deep cx t2) then Literal top
    else
      error cx ("Cannot unify " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)

let rec least_common_parent (cx : contexts) (t1 : typ) (t2 : typ) : typ =
  debug_print
    ( ">> least_common_parent"
    ^ string_of_pair (string_of_typ t1) (string_of_typ t2) ) ;
  if is_subtype cx t1 t2 then t2
  else if is_subtype cx t2 t1 then t1
  else least_common_parent cx (typ_step cx t1) t2

let least_common_parent_checked (cx : contexts) (t1 : typ) (t2 : typ) : typ =
  let t = least_common_parent cx t1 t2 in
  if is_illegal_typ cx t then
    error cx ("Cannot unify " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)
  else t

(* Checks that it is possible to resolve the
 * dimension expression under the given context *)
let rec check_dexp (cx : contexts) (d : dexp) : unit =
  match d with
  | DimNum _ -> ()
  | DimVar x ->
      get_frame cx x |> ignore_dexp ;
      ()
  | DimPlus (l, r) -> check_dexp cx l ; check_dexp cx r

let check_typ_valid (cx : contexts) (ogt : typ) : unit =
  let rec check_typ_valid_rec (t : typ) : unit =
    debug_print (">> check_typ_valid " ^ string_of_typ t) ;
    match t with
    | ParTyp (s, tl) ->
        ignore_typ (typ_step cx t) ;
        (* If this is an internal type, don't recurse on checking frames *)
        List.fold_left (fun _ -> check_typ_valid_rec) () tl
    | MemberTyp (c, o) -> ignore_typ (typ_step cx t)
    | Literal lt ->
        error cx
          "Cannot use literal type here. Can only use literal type as function \
           return type."
    | _ -> () in
  check_typ_valid_rec ogt

let rec typ_erase (cx : contexts) (t : typ) : TypedAst.etyp =
  debug_print (">> typ_erase " ^ string_of_typ t) ;
  let d_to_c opd =
    match opd with
    | DimNum i -> ConstInt i
    | DimVar s -> ConstVar s
    | _ -> error cx ("No valid concrete interpretation of " ^ string_of_typ t)
  in
  match t with
  | UnitTyp -> TypedAst.UnitTyp
  | BoolTyp -> TypedAst.BoolTyp
  | IntTyp -> TypedAst.IntTyp
  | FloatTyp -> TypedAst.FloatTyp
  | StringTyp -> TypedAst.StringTyp
  | ThisTyp ->
      debug_fail cx
        "Cannot erase 'this'.  Did you forget to erase it in the first pass?"
  | ArrTyp (t', d) -> TypedAst.ArrTyp (typ_erase cx t', d_to_c d)
  | ParTyp (s, tl) ->
      let is_ext =
        match get_typ_safe cx s with Some (ext, _, _) -> ext | None -> false
      in
      if is_ext then TypedAst.ParTyp (s, List.map (typ_erase cx) tl)
      else typ_erase cx (typ_step cx t)
  | MemberTyp _ | Literal _ -> typ_erase cx (primitive_deep cx t)
  | AutoTyp -> error cx "Cannot infer the type of auto"
  | AnyTyp -> TypedAst.AnyTyp
  | GenTyp -> TypedAst.GenTyp
  | BotTyp | AnyFrameTyp | FrameTyp _ | GenArrTyp _ | ExactCodeTyp ->
      debug_fail cx ("Cannot erase " ^ string_of_typ t)
  | StructureTyp -> TypedAst.StructureTyp
  | ClassTyp -> TypedAst.ClassTyp

let rec etyp_to_typ (e : TypedAst.etyp) : typ =
  debug_print ">> etyp_to_typ" ;
  match e with
  | TypedAst.UnitTyp -> UnitTyp
  | TypedAst.BoolTyp -> BoolTyp
  | TypedAst.IntTyp -> IntTyp
  | TypedAst.FloatTyp -> FloatTyp
  | TypedAst.StringTyp -> StringTyp
  | TypedAst.ParTyp (s, tl) -> ParTyp (s, List.map etyp_to_typ tl)
  | TypedAst.ArrTyp (t, c) ->
      ArrTyp
        ( etyp_to_typ t
        , match c with ConstInt i -> DimNum i | ConstVar v -> DimVar v )
  | TypedAst.AnyTyp -> AnyTyp
  | TypedAst.GenTyp -> GenTyp
  | TypedAst.ExactCodeTyp -> ExactCodeTyp
  | TypedAst.StructureTyp -> StructureTyp
  | TypedAst.ClassTyp -> ClassTyp

let rec check_val (cx : contexts) (v : value) : typ =
  debug_print (">> check_val " ^ string_of_value v) ;
  match v with
  | Bool b -> Literal BoolTyp
  | Num n -> Literal IntTyp
  | Float f -> Literal FloatTyp
  | StringVal s -> Literal StringTyp
  | Unit -> error cx ("Unexpected value " ^ string_of_value v)

let exp_to_texp (cx : contexts) ((exp, t) : TypedAst.exp * typ) : TypedAst.texp
    =
  debug_print (">> exp_to_texp " ^ string_of_typ t) ;
  (exp, typ_erase cx t)

(* Given a list of arguments and the arguments of a function 'target' *)
(* Attempts to produce a list of valid types for the parameterization of the function *)
let infer_pml (cx : contexts) (args : typ list) (target : params) :
    typ list option =
  debug_print ">> infer_pml" ;
  let update_inference (t : typ) (s : string) (fpm : typ Assoc.context option) :
      typ Assoc.context option =
    match fpm with
    | None -> None
    | Some p ->
        if Assoc.mem s p then
          let t' = least_common_parent cx t (Assoc.lookup s p) in
          if is_illegal_typ cx t' then None else Some (Assoc.update s t' p)
        else Some (Assoc.update s t p) in
  let rec unify_param (fpm : typ Assoc.context option) (arg_typ : typ)
      (par_typ : typ) : typ Assoc.context option =
    (* Only update our inference if we are working on an abstract type *)
    let is_abs s = Assoc.mem s cx.pm in
    let new_fpm s = if is_abs s then update_inference arg_typ s fpm else fpm in
    match (arg_typ, par_typ) with
    | ParTyp (_, tl1), ParTyp (s, tl2) ->
        (* Abstract params may have unspecified
         * parameterizations provided by the arguments *)
        if List.length tl1 != List.length tl2 then new_fpm s
        else List.fold_left2 unify_param (new_fpm s) tl1 tl2
    | MemberTyp (t1, t2), MemberTyp (t3, t4) ->
        unify_param (unify_param fpm t2 t4) t1 t3
    | _, ParTyp (s, _) -> new_fpm s
    | ArrTyp (t1, _), ArrTyp (t2, _) -> unify_param fpm t1 t2
    | _ -> fpm in
  let inferred =
    List.fold_left2 unify_param (Some Assoc.empty) args (List.map tr_snd target)
  in
  (* Correctly sort the produced parameter list *)
  match inferred with
  | None -> None
  | Some inf ->
      List.fold_right
        (fun x a ->
          match a with
          | None -> None
          | Some acc ->
              if Assoc.mem x inf then Some (Assoc.lookup x inf :: acc) else None)
        (Assoc.keys cx.pm) (Some [])

let try_fn_inv (cx : contexts) (x : id) (pml : typ list)
    (args : (TypedAst.exp * typ) list) (c : string option) (f : fn_typ) :
    (typ Assoc.context option * fn_typ * typ Assoc.context) option =
  let arg_typs = List.map snd args in
  let ml, rt, x, params, meta' = f in
  let pm = get_ml_pm cx ml in
  debug_print (">> try_fn_inv " ^ string_of_fn_typ f) ;
  (* This function asserts whether or not the function
   * invocation matches the function given *)
  (* In particular, this checks whether the given function
   * matches the given parameterization and parameters *)
  (* If it is valid, this returns (Some 'map from parameter to type'),
   * otherwise returns 'None' *)
  (* If we have the wrong number of arguments, then no match for sure *)
  if List.length args != List.length params then None
  else
    (* Work out the parameter inference if one is needed *)
    let inferred_pml =
      if Assoc.size pm == List.length pml then Some pml
      else if List.length pml == 0 then
        infer_pml (with_pm cx pm) arg_typs params
      else None in
    match inferred_pml with
    | None -> None
    | Some ipml -> (
        (* Check that the parameterization conforms to the bounds provided *)
        let ipml_clean =
          List.map (replace_abstype (Assoc.map fst cx.pm)) ipml in
        let param_check =
          match_parameterization_safe (with_pm cx pm) ipml_clean in
        let scheme_check =
          match c with
          | None -> None
          | Some scheme -> (
              let scx = with_pm cx (fst (get_scheme cx scheme)) in
              match infer_pml scx arg_typs params with
              | None -> None
              | Some spml -> match_parameterization_safe scx spml ) in
        match param_check with
        | None -> None
        | Some pm_map ->
            (* Get the parameters types and replace them in params_typ *)
            let param_typs = List.map tr_snd params in
            let param_typs' = List.map (replace_abstype pm_map) param_typs in
            let param_typs'' =
              match scheme_check with
              | None -> param_typs'
              | Some spm_map -> List.map (replace_abstype spm_map) param_typs'
            in
            (* TODO: something's wrong here: see the note at the top of canon_basics *)
            (* Finally, check that the arg and parameter types match *)
            if List.length arg_typs == List.length param_typs then
              Option.map
                (fun x -> (scheme_check, f, x))
                (List.fold_left2
                   (fun acc arg param ->
                     if is_subtype cx arg param then acc else None)
                   param_check arg_typs param_typs'')
            else None )

let check_fn_inv (cx : contexts) (x : id) (pml : typ list)
    (args : (TypedAst.exp * typ) list) :
    (string * TypedAst.etyp list * TypedAst.args) * typ =
  debug_print (">> check_fn_inv " ^ x) ;
  let arg_typs = List.map snd args in
  (* find definition for function in phi *)
  (* looks through all possible overloaded definitions of the function *)
  (* Check if this function should be treated as a scheme function *)
  let fn_invocated = get_functions_safe cx x in
  match
    List.fold_right
      (fun (c, f) acc ->
        match (acc, try_fn_inv cx x pml args c f) with
        | None, f -> f
        | Some _, None -> acc
        | Some f1, Some f2 ->
            let _, ft1, _ = f1 in
            let _, ft2, _ = f2 in
            let _, _, _, p1, _ = ft1 in
            let _, _, _, p2, _ = ft2 in
            (* If one of the functions is a subtype of the other, we can use it *)
            if
              List.fold_left2
                (fun acc (_, x, _) (_, y, _) -> is_subtype cx x y && acc)
                true p1 p2
            then Some f2
            else if
              List.fold_left2
                (fun acc (_, x, _) (_, y, _) -> is_subtype cx x y && acc)
                true p2 p1
            then Some f1
            else
              error cx
                ( "Ambiguous choice of functions to call" ^ string_of_fn_typ ft1
                ^ " and " ^ string_of_fn_typ ft2 ))
      fn_invocated None
  with
  | Some (spm, fn_found, pmt) ->
      let ml, rt, x', _, _ = fn_found in
      (* We fold_left to reverse the list order *)
      let pme =
        List.fold_left
          (fun acc (_, t) ->
            if is_subtype cx t AnyFrameTyp then acc else typ_erase cx t :: acc)
          [] (Assoc.bindings pmt) in
      let xr = if has_modification cx ml External then x else x' in
      let rt' =
        match spm with None -> rt | Some spmt -> replace_abstype spmt rt in
      ((xr, pme, List.map (exp_to_texp cx) args), replace_abstype pmt rt')
  | None ->
      error cx
        ( "No overloaded function declaration of " ^ x
        ^ ( if List.length pml > 0 then
            string_of_bounded_list string_of_typ "<" ">" pml
          else "" )
        ^ " matching types "
        ^ string_of_bounded_list string_of_typ "(" ")" arg_typs
        ^ " found" )

(* Checks the validity of a parameterizations,
 * and returns the contexts updated with that pm *)
let check_parameterization (cx : contexts) (pm : parameterization) : contexts =
  debug_print (">> check_parameterization " ^ string_of_parameterization pm) ;
  let check_parameter found (s, (t, b)) =
    if Assoc.mem s found then error cx ("Duplicate parameter `" ^ s)
    else check_typ_valid (with_pm cx found) t ;
    Assoc.update s (t, b) found in
  ignore_typ_bool_context
    (List.fold_left check_parameter Assoc.empty (List.rev (Assoc.bindings pm))) ;
  with_pm cx pm

let update_psi (cx : contexts) (f : fn_typ) : contexts =
  (* Update psi, raising errors in case of a duplicate *)
  (* If the given type is not valid in psi, psi is returned unmodified *)
  (* Will raise a failure if a non-concrete vartyp is used *)
  let ml, rt, id, pr, _ = f in
  if not (has_modification cx ml Canon) then cx
  else
    let fail _ = error cx ("Invalid canonical function " ^ string_of_fn_typ f) in
    debug_print (">> update_psi " ^ string_of_fn_typ f) ;
    let target = rt in
    let prc =
      List.fold_right
        (fun (ml, t, _) acc ->
          if not (has_modification cx ml Canon) then t :: acc else acc)
        pr [] in
    if List.length prc != 1 then fail ()
    else
      let start = List.hd prc in
      let is_valid (t : typ) : bool =
        match t with MemberTyp _ -> true | _ -> false in
      if (not (is_valid start)) || not (is_valid target) then fail ()
      else
        let as_geo_typ (t : typ) : (string * string) option =
          match t with
          | MemberTyp (ParTyp (c, _), ParTyp (o, _)) -> Some (c, o)
          | _ -> None in
        match as_geo_typ start with
        | None -> fail ()
        | Some (c1, o1) -> (
          match as_geo_typ target with
          | None -> fail ()
          | Some (c2, o2) ->
              let start_string = string_of_typ start in
              if Assoc.mem start_string cx.ps then
                let start_fns = Assoc.lookup start_string cx.ps in
                with_ps cx (Assoc.update start_string (id :: start_fns) cx.ps)
              else with_ps cx (Assoc.update start_string [id] cx.ps) )

(* Type check parameter; check parameter typ validity *)
(* Returns gamma *)
let check_param (cx : contexts) ((ml, t, id) : modification list * typ * string)
    : contexts =
  debug_print ">> check_param" ;
  check_typ_valid cx t ;
  bind_typ cx id ml t

(* Get list of parameters from param list *)
(* Returns gamma *)
let check_params (cx : contexts) (pl : params) : contexts * TypedAst.params =
  debug_print ">> check_params" ;
  let cx' = List.fold_left check_param cx pl in
  let p = List.map (fun (ml, t, x) -> (get_ml_sq cx ml, typ_erase cx t, x)) pl in
  (cx', p)

let check_index_exp (cx : contexts) (t1 : typ) (t2 : typ) : typ =
  match (primitive cx t1, primitive cx t2) with
  | ArrTyp ((ArrTyp _ as t), _), IntTyp -> Literal t
  | ArrTyp (t, _), IntTyp -> t
  | _ ->
      error cx
        ( "Expected array and integer for indexing, got " ^ string_of_typ t1
        ^ " and " ^ string_of_typ t2 )

let check_as_exp (cx : contexts) (start : typ) (target : typ) : typ =
  ignore_typ (least_common_parent_checked cx start target) ;
  target

(* Super expensive.  We're essentially relying on small contexts *)
let find_in_path (cx : contexts) (start_exp : aexp) (start : typ) (target : typ)
    : aexp =
  debug_print
    (">> find_in_path " ^ string_of_typ start ^ " " ^ string_of_typ target) ;
  let rec psi_path_rec (to_search : (typ * aexp) Queue.t) (found : typ list) :
      aexp =
    (* Given a type and the psi elements associated with that type *)
    (* Returns a list of types reachable by using available canonical functions *)
    (* Note that the string list give the arguments to the canonical function,
     * where the empty string is a special case for the previous in-expression value *)
    let search_phi (tl : typ) (ps_lst : string list) :
        (typ * fn_inv * string option list) list =
      (* This function searches phi for canonical abstract functions that map from the given type *)
      (* A list of the types these functions map with the inferred type parameters is returned *)
      (* If multiple functions are possible, then ambiguities are resolved with the following priorities *)
      (* 1. Minimize upcasting requirements (actually handled by use of this function) *)
      (* 2. Minimize number of type parameters *)
      (* 3. Minimize constraint bounds *)
      let rec search_fn ((c, ft) : string option * fn_typ)
          (args : string option list) :
          (typ * (id * typ list * typ list) * string option list) list =
        let ml, rt, id, params, _ = ft in
        debug_print (">> search_fn " ^ id) ;
        (* Get the next index to modify *)
        (* Is a negative number if all non-canon elements are already set *)
        let next_index =
          List.fold_left2
            (fun acc (ml, _, _) arg ->
              if acc > 0 then acc
              else if has_modification cx ml Canon && Option.is_none arg then
                -acc
              else acc - 1)
            (-1) params args
          - 1 in
        let pm = get_ml_pm cx ml in
        let cxf = with_pm cx pm in
        if next_index >= 0 then
          (* Don't even try to eliminate non sub-types cause it's a pain *)
          (* Is needlessly slow as a result *)
          List.fold_right
            (fun x acc ->
              search_fn (c, ft) (list_replace (Some x) args next_index) @ acc)
            (get_canonical_vars cx) []
        else
          let args' =
            List.map
              (fun x -> match x with None -> tl | Some s -> get_var cx s)
              args in
          match infer_pml cxf args' params with
          | None -> []
          | Some pml -> (
            match match_parameterization_safe cxf pml with
            | None -> []
            | Some mpm -> (
                let pr1 = List.map (fst |- snd) (Assoc.bindings pm) in
                let rtr = replace_abstype mpm rt in
                let ptr = List.map (replace_abstype mpm |- tr_snd) params in
                let scheme_check =
                  match c with
                  | None -> None
                  | Some scheme -> (
                      let scx = with_pm cx (fst (get_scheme cx scheme)) in
                      match infer_pml scx args' params with
                      | None -> None
                      | Some spml -> match_parameterization_safe scx spml )
                in
                let ptr' =
                  match scheme_check with
                  | None -> ptr
                  | Some spm_map -> List.map (replace_abstype spm_map) ptr in
                let fail id2 s =
                  error cxf
                    ( "Ambiguity between viable canonical functions " ^ id
                    ^ " and " ^ id2 ^ " (" ^ s ^ ")" ) in
                let compare_parameterizations (acc : bool option) t1 t2 :
                    bool option =
                  let result = is_subtype cxf t1 t2 in
                  match acc with
                  | None -> Some result
                  | Some b ->
                      if b = result then acc
                      else
                        error cxf
                          ( "Ambiguous constraint ordering between "
                          ^ string_of_typ t1 ^ " and " ^ string_of_typ t2 )
                in
                if not (is_subtype_list cxf args' ptr') then []
                else
                  match rtr with
                  | MemberTyp _ ->
                      let rec_result = [] in
                      if
                        List.fold_left
                          (fun acc (rt, _, _) -> is_typ_eq cx rt rtr || acc)
                          false rec_result
                      then
                        List.map
                          (fun (rt, (id2, pml2, pr2), args) ->
                            if
                              List.length pr1 = List.length pr2
                              && List.length pr1 = 0
                            then
                              fail id2
                                ( "duplicate concrete paths from "
                                ^ string_of_typ tl ^ " to " ^ string_of_typ rtr
                                )
                            else if not (is_typ_eq cxf rt rtr) then
                              (rt, (id2, pml2, pr2), args)
                            else if List.length pr1 < List.length pr2 then
                              (rt, (id, pml, pr1), args)
                            else if List.length pr2 < List.length pr1 then
                              (rt, (id2, pml2, pr2), args)
                            else if
                              match
                                List.fold_left2 compare_parameterizations None
                                  pr1 pr2
                              with
                              | None ->
                                  debug_fail cxf
                                    "Unexpected concrete function type \
                                     duplicates in phi"
                              | Some b -> b
                            then (rt, (id2, pml2, pr2), args)
                            else (rtr, (id, pml, pr1), args))
                          rec_result
                        (* No duplicate type result found, just add this function to the list *)
                      else (rtr, (id, pml, pr1), args) :: rec_result
                  | _ ->
                      debug_fail cxf
                        ( "Canonical function " ^ id ^ " resulted in type "
                        ^ string_of_typ rtr
                        ^ ", while canonical functions should always result in \
                           a coordtyp" ) ) ) in
      let rec search_fns (fns : (string option * fn_typ) list) :
          (typ * (id * typ list * typ list) * string option list) list =
        match fns with
        | [] -> []
        | (c, fn) :: t ->
            let ml, _, _, params, _ = fn in
            if not (has_modification cx ml Canon) then search_fns t
            else if
              List.fold_right
                (fun (ml, _, _) acc ->
                  if not (has_modification cx ml Canon) then acc + 1 else acc)
                params 0
              != 1
            then debug_fail cx "Permitted function with not-1 non-canon param"
            else
              let start_args =
                List.fold_right (fun _ -> List.cons None) params [] in
              search_fn (c, fn) start_args @ search_fns t in
      let rec get_valid_fn (fns : (string option * fn_typ) list) :
          string option * fn_typ =
        List.hd fns in
      let rec search_phi_rec
          (fns : (string * (string option * fn_typ) list) list) :
          (typ * (id * typ list * 'a list) * string option list) list =
        match fns with
        | [] ->
            List.map
              (fun s ->
                let c, (ml, rt, id, pr, _) =
                  get_valid_fn (get_functions_safe cx s) in
                (rt, (id, List.map fst (Assoc.values (get_ml_pm cx ml)), []), []))
              ps_lst
        | (_, fs) :: t -> search_fns fs @ search_phi_rec t in
      (* TODO: using _bindings here is kinda janky, but it's hard to fix rn, so... *)
      List.map
        (fun (t, (x, y, _), z) -> (t, (x, y), z))
        (search_phi_rec (Assoc.bindings cx._bindings.p)) in
    let rec psi_lookup_rec (nt : typ) : (typ * fn_inv * string option list) list
        =
      (* NOTE: paths which would send to a type with more than
       * 5 generic levels are rejected to avoid infinite explosion *)
      let rec check_typ_ignore (t : typ) (count : int) : bool =
        if count > 5 then true
        else
          match t with
          | MemberTyp (_, ParTyp (_, tl)) ->
              List.fold_left
                (fun acc t -> acc || check_typ_ignore t (count + 1))
                false tl
          | _ -> false in
      if check_typ_ignore nt 0 then []
      else
        let s_lookup = string_of_typ nt in
        let ps_lst =
          if Assoc.mem s_lookup cx.ps then Assoc.lookup s_lookup cx.ps else []
        in
        let to_return = search_phi nt ps_lst in
        let next_step =
          match nt with MemberTyp _ -> typ_step cx nt | _ -> nt in
        match next_step with
        | MemberTyp _ -> to_return @ psi_lookup_rec next_step
        | _ -> to_return in
    let rec update_search_and_found
        (vals : (typ * fn_inv * string option list) list) (e : aexp) : typ list
        =
      match vals with
      | [] -> found
      | (t1, (v, pml), exps) :: t ->
          if
            List.fold_left (fun acc t2 -> acc || is_typ_eq cx t1 t2) false found
          then update_search_and_found t e
          else
            (* Erase the specific invocation found above for future typechecking *)
            (* This is a hack that can probably get removed in favor of not typechecking the (already found) result *)
            (* This is the reason we need the hack of adding _0 to functions ending with a _number name *)
            let r = Str.regexp ".*_[0-9]+" in
            let v' = if Str.string_match r v 0 then String.sub v 0 (String.rindex v '_') else v in
            let aes =
              List.map
                (fun so -> match so with None -> e | Some s -> (Var s, snd e))
                exps in
            let e' = (FnInv (v', pml, aes), snd e) in
            (* Note the update to the stateful queue *)
            Queue.push (t1, e') to_search ;
            t1 :: update_search_and_found t e in
    let nt, e =
      if Queue.is_empty to_search then
        error cx
          ( "Cannot find a path from " ^ string_of_typ start ^ " to "
          ^ string_of_typ target )
      else Queue.pop to_search in
    if is_subtype cx nt target then e
    else psi_path_rec to_search (update_search_and_found (psi_lookup_rec nt) e)
  in
  if is_typ_eq cx start target then start_exp
  else
    let q = Queue.create () in
    Queue.push (start, start_exp) q ;
    psi_path_rec q []

let check_in_exp (cx : contexts) (start_exp : aexp) (start : typ) (target : typ)
    : aexp =
  debug_print ">> check_in_exp" ;
  let fail _ =
    error cx
      ( "Invalid type as 'in' target " ^ string_of_typ target
      ^ ", expected a scheme or frame" ) in
  let c, p, o =
    match start with
    | MemberTyp (ParTyp (c, p), o) -> (c, p, o)
    | _ ->
        error cx
          ( "Invalid application of 'in' to type " ^ string_of_typ start
          ^ ", expected a geometric type" ) in
  let target' =
    match target with
    | ParTyp (s, tl) -> (
      match find_typ cx s with
      | Some (Delta _) -> MemberTyp (ParTyp (c, [target]), o)
      | Some (Chi _) ->
          let tl' = if List.length tl == 0 then p else tl in
          MemberTyp (ParTyp (s, tl'), o)
      | _ -> fail () )
    | _ -> fail () in
  find_in_path cx start_exp start target'

let check_swizzle (cx : contexts) (args : (TypedAst.exp * typ) list) :
    TypedAst.exp * typ =
  match args with
  | [(arr, ArrTyp (ArrTyp _, _)); (Val (StringVal s), Literal _)]
   |[(arr, ArrTyp (Literal (ArrTyp _), _)); (Val (StringVal s), Literal _)]
   |[(arr, Literal (ArrTyp (ArrTyp _, _))); (Val (StringVal s), Literal _)]
   |[ (arr, Literal (ArrTyp (Literal (ArrTyp _), _)))
    ; (Val (StringVal s), Literal _) ] ->
      error cx "You cannot swizzle on a matrix "
  | [((arr, ArrTyp (t, _)) as tarr); (Val (StringVal s), Literal _)]
   |[((arr, Literal (ArrTyp (t, _))) as tarr); (Val (StringVal s), Literal _)]
    -> (
    try
      let indices =
        List.init (String.length s) (fun i -> s.[i] |> char_to_index) in
      let index_type = typ_erase cx t in
      let create_index i : TypedAst.exp =
        Index (exp_to_texp cx tarr, (Val (Num (List.nth indices i)), IntTyp))
      in
      if List.length indices == 1 then (create_index 0, t)
      else
        ( Arr (List.map (fun i -> (create_index i, index_type)) indices)
        , Literal (ArrTyp (t, DimNum (List.length indices))) )
    with Failure s -> error cx ("We had an error in swizzling: " ^ s) )
  | _ ->
      error cx
        ( "Expected array and string for swizzling, got "
        ^ ( List.map (fun (_, typ) -> string_of_typ typ) args
          |> String.concat " and " ) )

let rec check_aexp (cx : contexts) ((e, meta) : aexp) : TypedAst.exp * typ =
  check_exp (with_meta cx meta) e

and check_exp (cx : contexts) (e : exp) : TypedAst.exp * typ =
  debug_print (">> check_exp " ^ string_of_exp e) ;
  match e with
  | Val v -> (TypedAst.Val v, check_val cx v)
  | Var v -> (TypedAst.Var v, get_var cx v)
  | Arr a -> check_arr cx a
  | As (e', t) ->
      let er, tr = check_aexp cx e' in
      (er, check_as_exp cx tr t)
  | In (e', t) ->
      let _, tr = check_aexp cx e' in
      check_aexp cx (check_in_exp cx e' tr t)
  | Index (l, r) ->
      let el = check_aexp cx l in
      let er = check_aexp cx r in
      ( TypedAst.Index (exp_to_texp cx el, exp_to_texp cx er)
      , check_index_exp cx (snd el) (snd er) )
  | FnInv (x, pr, args) ->
      if String.equal x "super" then (
        let c = most_recent_class cx in
        let (_, parent, _, _) = c in
        (match parent with
        | None -> error cx "'super' used in a class with no parent class"
        | Some par -> 
          (match class_method_lookup_shallow (get_class cx par) "init" with
          | Some (Method (vis, (fn_typ, _))) ->  (
            let typed_args1 = List.map (check_aexp cx) args in
            let typed_args2 = List.map (exp_to_texp cx) typed_args1 in
            match try_fn_inv cx x pr typed_args1 None fn_typ with
            | Some _ ->
              let _, ret_typ, _, _, _ = fn_typ in
              let typed_pr = List.map (typ_erase cx) pr in
              let class_name, _, _, _ = c in
              (TypedAst.MethodInv (None, x, typed_pr, typed_args2, class_name), ret_typ)
            | None -> error cx ("Invalid method invocation: " ^ x)
          )
          | _ -> error cx "parent type has no constructor"
          )
        )
      )
      else (
        let (a, b, c), t = check_fn_inv cx x pr (List.map (check_aexp cx) args) in
        (TypedAst.FnInv (a, b, c), t)
      )
  | MethodInv (exp, x, pr, args) -> (
    let c, typed_exp = match exp with
    | Some e -> (
      let (typed_e, t) = check_exp cx e in
      match class_of_typ cx t with
      | Some c' -> (c', Some typed_e)
      | None -> error cx ("Invalid method invocation: " ^ x)
    )
    | None -> (most_recent_class cx, None) in
    match class_method_lookup_deep cx c x with
    | Some (Method (vis, (fn_typ, _)), _, _) -> (
      let typed_args1 = List.map (check_aexp cx) args in
      let typed_args2 = List.map (exp_to_texp cx) typed_args1 in
      match try_fn_inv cx x pr typed_args1 None fn_typ with
      | Some _ ->
        let _, ret_typ, _, _, _ = fn_typ in
        let typed_pr = List.map (typ_erase cx) pr in
        let class_name, _, _, _ = c in
        (TypedAst.MethodInv (typed_exp, x, typed_pr, typed_args2, class_name), ret_typ)
      | None -> error cx ("Invalid method invocation: " ^ x)
    )
    | _ -> error cx ("Invalid method invocation: " ^ x))
  | FieldSelect (Some e', s) -> ( (* can be struct, class, or swizzle *)
    let (typed_e, t) = check_exp cx e' in
    match structure_of_typ cx t with
    | Some st -> ( (* Treat as struct field select *)
        match struct_field_lookup st s with
        | Some field_typ ->
            (TypedAst.FieldSelect (Some typed_e, s, ""), field_typ)
        | None ->
            error cx ("Invalid field name " ^ s))
    | None -> (
        match class_of_typ cx t with
        | Some c -> ( (* Treat as class field select *)
          let (class_name, _, _, _) = c in
          match class_field_lookup_deep cx c s with
            | Some (Field (_, field_type, _), _) ->
                (TypedAst.FieldSelect (Some typed_e, s, class_name), field_type)
            | _ -> raise (TypeException ("Invalid field " ^ s)))
        | None -> ( (* Treat as swizzle *)
          let args = [(typed_e, t); check_exp cx (Val (StringVal(s)))] in
          try check_swizzle cx args
          with TypeException e -> (
            match args with
            (* See if this is actually something like mesh.normals in which case treat it like one var *)
            | [(Var s1, _); (Val (StringVal s2), _)] ->
                check_exp cx (Var (s1 ^ "." ^ s2))
            | _ -> raise (TypeException e) ))))
    | FieldSelect (None, s) -> (
        let c = most_recent_class cx in
        let (class_name, _, _, _) = c in
        match class_field_lookup_deep cx c s with
        | Some (Field (_, field_type, _), _) -> (TypedAst.FieldSelect (None,
            s, class_name), field_type)
        | _ -> raise (TypeException ("Invalid field " ^ s)))

and check_arr (cx : contexts) (a : aexp list) : TypedAst.exp * typ =
  debug_print ">> check_arr" ;
  let a' = List.map (check_aexp cx) a in
  ( TypedAst.Arr (List.map (exp_to_texp cx) a')
  , Literal
      (ArrTyp
         ( List.fold_left
             (fun acc (_, t) -> least_common_parent_checked cx acc t)
             BotTyp a'
         , DimNum (List.length a) )) )

(* Updates Gamma and Psi *)
let rec check_acomm (cx : contexts) ((c, meta) : acomm) :
    contexts * TypedAst.comm =
  check_comm (with_meta cx meta) c

(* Updates Gamma and Psi *)
and check_comm (cx : contexts) (c : comm) : contexts * TypedAst.comm =
  debug_print (">> check_comm " ^ string_of_comm c) ;
  match c with
  | Skip -> (cx, TypedAst.Skip)
  | Print e -> (
      let e, t = exp_to_texp cx (check_aexp cx e) in
      match t with
      | UnitTyp -> error cx "Print function cannot print void types"
      | _ -> (cx, TypedAst.Print (e, t)) )
  | Exp e -> (cx, TypedAst.Exp (exp_to_texp cx (check_aexp cx e)))
  | Decl (ml, t, s, e) ->
      check_typ_valid cx t ;
      let result = check_aexp cx e in
      let t' =
        match t with
        | AutoTyp -> (
            let t' = snd result in
            match t' with
            | FrameTyp _ | AnyFrameTyp ->
                error cx ("Cannot write " ^ string_of_typ t' ^ " to auto")
            | Literal _ | BotTyp ->
                error cx ("Cannot infer the type of " ^ string_of_aexp e)
            | _ -> t' )
        | _ -> t in
      check_assign cx t' (Var s) (snd result) ;
      ( bind_typ cx s ml t'
      , TypedAst.Decl (typ_erase cx t', s, exp_to_texp cx result) )
  | Assign (s, e) ->
      let x, t = check_aexp cx s in
      let result = check_aexp cx e in
      check_assign cx t (fst s) (snd result) ;
      (cx, TypedAst.Assign (exp_to_texp cx (x, t), exp_to_texp cx result))
  | AssignOp (s, b, e) -> (
      let cx', c' =
        check_acomm cx
          (Assign (s, (FnInv (b, [], [(fst s, snd e); e]), cx.meta)), cx.meta)
      in
      match c' with
      | TypedAst.Assign (s', (TypedAst.FnInv (_, _, [(_, st); e]), _)) ->
          (cx', TypedAst.AssignOp (s', b, e))
      | _ -> debug_fail cx "Assign must return an assign?" )
  | If ((b, c1), el, c2) ->
      let check_if b c =
        let er = check_aexp cx b in
        let _, cr = check_comm_lst cx c in
        if is_subtype cx (snd er) BoolTyp then (exp_to_texp cx er, cr)
        else error cx "Expected boolean expression for if condition" in
      let c2r =
        match c2 with
        | Some e -> Some (snd (check_comm_lst cx e))
        | None -> None in
      ( cx
      , TypedAst.If
          (check_if b c1, List.map (fun (b, c) -> check_if b c) el, c2r) )
  | For (c1, b, c2, cl) ->
      let cx', c1r = check_acomm cx c1 in
      let br, brt = check_aexp cx' b in
      let btexp = exp_to_texp cx (br, brt) in
      let cx'', c2r = check_acomm cx' c2 in
      (cx, TypedAst.For (c1r, btexp, c2r, snd (check_comm_lst cx'' cl)))
  | Return e ->
      (cx, TypedAst.Return (Option.map (exp_to_texp cx |- check_aexp cx) e))
  | ExactCodeComm ec -> (cx, TypedAst.ExactCodeComm ec)

(* Updates Gamma and Psi *)
and check_comm_lst (cx : contexts) (cl : acomm list) :
    contexts * TypedAst.comm list =
  debug_print ">> check_comm_lst" ;
  match cl with
  | [] -> (cx, [])
  | h :: t ->
      let cx', c' = check_acomm cx h in
      let cx'', cl' = check_comm_lst cx' t in
      (cx'', c' :: cl')

(* Updates Gamma *)
and check_assign (cx : contexts) (t : typ) (x : exp) (etyp : typ) : unit =
  debug_print
    ( ">> check_assign " ^ string_of_typ t ^ " " ^ string_of_exp x
    ^ " assigned " ^ string_of_typ etyp ) ;
  (* Check that t, if not a core type, is a registered tag *)
  let rec check_tag (t : typ) : unit =
    match t with
    | ParTyp _ ->
        typ_step cx t |> ignore_typ ;
        ()
    | _ -> () in
  check_tag t ;
  if is_subtype cx etyp t then ()
  else
    error cx
      ( "Mismatched types for var decl for " ^ string_of_exp x ^ ": expected "
      ^ string_of_typ t ^ ", found " ^ string_of_typ etyp )

(* Helper function for type checking void functions.
 * Functions that return void can have any number of void return statements
 * anywhere. *)
let check_void_return (cx : contexts) (c : acomm) : unit =
  debug_print ">> check_void_return" ;
  match c with
  | Return (Some _), _ -> error cx "Void functions cannot return a value"
  | _ -> ()

let check_return (cx : contexts) (t : typ) (c : acomm) : unit =
  debug_print ">> check_return" ;
  match (t, c) with
  | UnitTyp, (Return None, meta) -> ()
  | _, (Return None, meta) ->
      error (with_meta cx meta) "Expected a return value instead of void"
  | UnitTyp, (Return (Some _), meta) ->
      error (with_meta cx meta) "Void functions cannot return a value"
  | _, (Return (Some r), meta) ->
      let cx' = with_meta cx meta in
      let _, rt = check_aexp cx' r in
      (* raises return exception of given boolean exp is false *)
      if is_subtype cx' rt t then ()
      else
        error cx'
          ( "Mismatched return types, expected: " ^ string_of_typ t
          ^ ", found: " ^ string_of_typ rt )
  | _ -> ()

let rec check_valid_supertype (cx : contexts) (cx' : contexts) (t : typ) : typ =
  match t with
  | AnyTyp | BoolTyp | IntTyp | FloatTyp | StringTyp -> t
  | ArrTyp (t', _) -> check_valid_supertype cx cx' t'
  (*| MemberTyp (mem_c, mem_t) ->*)
  | MemberTyp (ParTyp (s1, pml1), ParTyp (s2, pml2)) ->
      (* Matches something of the form "cart3<model>.point" *)
      (* s1 is cart3, pml1 is [model], s2 is point, pml2 is [] *)
      let tpm_scheme = match get_scheme_safe cx' s1 with
      | Some c -> fst c
      | None -> error cx ("Bad type declaration: Invalid scheme " ^ s1) in
      let frame_typ = List.hd pml1 in
      let _ = match frame_typ with
      | ParTyp (s, pml) -> (match get_frame_safe cx' s with
        | Some d -> ()
        | None -> error cx ("Bad type declaration: Invalid frame " ^ s))
      | _ -> error cx ("Bad type declaration: Invalid frame " ^ (string_of_typ frame_typ)) in
      let tpm_object = match get_typ_safe cx' (s1 ^ "." ^ s2) with
      | Some (_, tpm', _) -> tpm'
      | None -> error cx ("Bad type declaration: Invalid object " ^ s2) in
      check_valid_supertype_helper cx cx' false s1 t pml1 tpm_scheme;
      check_valid_supertype_helper cx cx' false s2 t pml2 tpm_object;
      t
  | ParTyp (s, pml) ->
      if Assoc.mem s cx'.pm then
        check_valid_supertype cx cx' (fst (Assoc.lookup s cx'.pm)) 
      else
        let (_, tpm, _) = get_typ cx' s in
        check_valid_supertype_helper cx cx' false s t pml tpm;
        t
  | _ -> error cx ("Invalid type declaration " ^ string_of_typ t)

(* this factors out the tpm-dependent code *)
and check_valid_supertype_helper (cx : contexts) (cx' : contexts) (recurse : bool)
    (s : string) (t : typ) (pml : typ list) (tpm : parameterization) : unit =
    let pmb = Assoc.bindings tpm in
    if List.length pmb == List.length pml then (
      List.fold_left2
        (fun acc (s, c) t' ->
        if is_subtype cx' t' c then ()
        else
          error cx
            ( "Invalid typ used in the parameterization "
            ^ string_of_typ t ^ " for parameter " ^ s ))
      ()
      (List.map (fun (s, (t, _)) -> (s, t)) (Assoc.bindings tpm))
      (if recurse then (List.map (check_valid_supertype cx cx') pml) else pml) ;
      ) 
    else
      error cx
        ( "Invalid number of parameters\n\
          \                provided to parameterized type " ^ s )

(* Updates Tau with new typing information *)
and check_typ_decl (cx : contexts) (x : string) ((b, pm, t) : tau) : contexts =
  debug_print ">> check_typ_decl" ;
  let cx' = with_pm cx pm in
  check_valid_supertype cx cx' t |> ignore_typ ;
  bind cx x (Tau (b, pm, t))

(* Updates Phi, and internal calls update gamma and psi *)
let check_fn_decl (cx : contexts) (f : fn_typ) :
    contexts * (TypedAst.params * TypedAst.parameterization) option =
  let ml, rt, id, pl, meta = f in
  let pm = get_ml_pm cx ml in
  debug_print (">> check_fn_decl : " ^ id ^ string_of_parameterization pm) ;
  let cx' = check_parameterization cx pm in
  let cx'', pr = check_params cx' pl in
  let pme =
    List.fold_right
      (fun (s, t) acc ->
        if is_subtype cx'' t AnyFrameTyp then acc
        else Assoc.update s (typ_erase cx'' t) acc)
      (List.map (fun (s, (t, _)) -> (s, t)) (Assoc.bindings pm))
      Assoc.empty in
  if has_modification cx ml External then (cx'', None)
  else (cx'', Some (pr, pme))

(* Updates phi and psi *)
let check_fn (cx : contexts) ((f, cl) : fn) (scheme : string option) :
    contexts * TypedAst.fn option =
  let ml, rt, id, pl, meta = f in
  debug_print (">> check_fn : " ^ id) ;
  (* update phi with function declaration *)
  let cx', ft = check_fn_decl cx f in
  (* Note that we don't use our updated phi to avoid recursion *)
  let cx'', cl' = check_comm_lst cx' cl in
  let id', cxf = bind_function cx f scheme in
  let cxr = update_psi cxf f in
  (* check that the last command is a return statement *)
  (* TODO: might want to check that there is exactly
   * one return statement on each branch *)
  List.iter (check_return cx'' rt) cl ;
  ( cxr
  , Option.map (fun (tpm, tpr) -> ((typ_erase cx' rt, id', tpr, tpm), cl')) ft
  )

(* Type check global variable *)
(* Updates gamma *)
let check_global_variable (cx : contexts) ((ml, t, id, e) : global_var) :
    contexts * TypedAst.global_var option =
  debug_print ">> check_global_variable" ;
  check_typ_valid cx t ;
  let e' = Option.map (fun x -> check_aexp cx x) e in
  (match e' with Some (_, te) -> check_assign cx t (Var id) te | None -> ()) ;
  let gvr =
    if has_modification cx ml External then None
    else 
    Some (get_ml_sq cx ml, typ_erase cx t, id, Option.map (fun x -> exp_to_texp cx x) e')
  in
  (bind_typ cx id ml t, gvr)

let check_frame (cx : contexts) ((id, d) : frame) : contexts =
  debug_print (">> check_frame " ^ id) ;
  check_dexp cx d ;
  bind cx id (Delta d)

(* Helper function for the rewriting pass
 * applied to all commands in a scheme definition *)
let rewrite_scheme_typ (cx : contexts) (scheme : id) : typ -> typ =
  let f (st : id) (spm : typ list) (t : typ) : typ =
    let rec map_typ_rec t' =
      match t' with
      | ParTyp (s, pm) -> (
          let pm' = List.map map_typ_rec pm in
          match get_typ_safe cx s with
          | Some _ -> ParTyp (s, pm')
          | None -> (
            match get_typ_safe cx (st ^ "." ^ s) with
            | Some _ -> MemberTyp (ParTyp (st, spm), ParTyp (s, pm'))
            (* Don't fail yet, could be an abstract type *)
            | None -> ParTyp (s, pm') ) )
      | MemberTyp (ThisTyp, t2) -> MemberTyp (ParTyp (st, spm), t2)
      | MemberTyp (ParTyp (s, pm), t2) ->
          if not (s = "this") then t else MemberTyp (ParTyp (st, pm), t2)
      | MemberTyp (t1, t2) -> MemberTyp (map_typ_rec t1, map_typ_rec t2)
      | ArrTyp (tl, a) -> ArrTyp (map_typ_rec tl, a)
      | GenArrTyp t'' -> GenArrTyp (map_typ_rec t'')
      | _ -> t' in
    map_typ_rec t in
  match get_scheme cx scheme with
  | pm, None -> f scheme []
  | pm, Some _ -> f scheme (List.map (fun s -> ParTyp (s, [])) (Assoc.keys pm))

let rewrite_scheme_fn_inv (cx : contexts) (scheme : id) (s : string) : string =
  match find_exp cx s with
  | Some _ -> s
  | None -> (
    match find_typ cx s with Some _ -> s | None -> scheme ^ "." ^ s )

(* Updates tau or phi with the prototype element being checked *)
let check_prototype_element (cx : contexts) (p : string)
    (pe : prototype_element) : contexts =
  debug_print (">> check_prototype_element " ^ string_of_prototype_element pe) ;
  match pe with
  | ProtoObject (ml, id, t) ->
      let pm = get_ml_pm cx ml in
      bind cx
        (p ^ "." ^ id)
        (Tau (false, pm, match t with Some t' -> t' | None -> AnyTyp))
  (* We don't actually generate erased prototype functions, just typecheck them *)
  | ProtoFn f ->
      let ml, rt, _, pr, _ = f in
      if
        has_modification cx ml Canon
        (* TODO: This should actually probably be fixed so that prototype canon
         * functions are loaded in as 'interfaces' but not used *)
      then error cx "Cannot have a canonical function in a prototype"
      else
        let cx' = with_pm cx (get_ml_pm cx ml) in
        check_typ_valid cx' rt ;
        List.fold_left (fun acc -> check_typ_valid cx' |- tr_snd) () pr ;
        snd (bind_function cx (rename_fn (fun x -> p ^ "." ^ x) f) (Some p))

let check_aprototype_element cx p ape : contexts =
  let pe', meta =
    map_aprototype_element cx
      (rewrite_scheme_fn_inv cx p)
      (fun x -> x)
      (rewrite_scheme_typ cx p) ape in
  check_prototype_element (with_meta cx meta) p pe'

(* Updates tau or phi with the coordinate scheme element being checked *)
(* We assume that  *)
let check_coordinate_element (cx : contexts) (c : string)
    (ce : coordinate_element) : contexts * TypedAst.fn option =
  debug_print (">> check_coordinate_element " ^ string_of_coordinate_element ce) ;
  let _, proto = get_scheme cx c in
  let proto =
    match proto with
    | Some s -> s
    | _ -> error cx ("Coordinate scheme " ^ c ^ " does not extend a prototype")
  in
  match ce with
  | CoordObjectAssign (ml, id, t) ->
      (* Check that the object is declared in the underlying prototype *)
      let _, s, _ = get_typ cx (proto ^ "." ^ id) in
      let fl = Assoc.bindings (get_ml_pm cx ml) in
      let s = Assoc.keys s in
      if List.length s != List.length fl then
        error cx
          ( id
          ^ " does not have the same number of\n\
            \                frame parameterizations as in " ^ proto ) ;
      (* Check that the object has a resolvable type *)
      let pm = Assoc.create (List.map (fun x -> x) fl) in
      check_typ_valid (with_pm cx pm) t ;
      (bind cx (c ^ "." ^ id) (Tau (false, pm, t)), None)
  | CoordFn fn ->
      let f, cl = fn in
      let ml, rt, id, pr, meta = f in
      let pm = get_ml_pm cx ml in
      let cxpm = with_pm cx pm in
      (* Check associated functions in the prototype to see if any match *)
      let fns = get_functions_safe cx (proto ^ "." ^ id) in
      (* TODO: To fix this, we need to do pml inference, and it's a huge pain *)
      let has_binding = true in
      (* If there's no expected declaration, then this is an internal function *)
      (* List.length fns = 0 ||
         List.fold_right (fun (_, (ml,prt,_,ppr,_)) acc ->
         acc || (is_subtype cxpm rt prt &&
         is_subtype_list cxpm (List.map fst pr) (List.map fst ppr)))
         fns false
         in *)
      if not has_binding then
        error cx
          ( "The type of function " ^ c ^ "." ^ id
          ^ " does not match any prototype definition of " ^ id ) ;
      (* Naming hack to make functions that aren't in the prototype 'internal' *)
      let fn' =
        if List.length fns = 0 then
          (rename_fn (fun x -> c ^ "." ^ x) (ml, rt, id, pr, meta), cl)
        else fn in
      let cx', tfn = check_fn cxpm fn' (Some c) in
      (cx', tfn)

let check_acoordinate_element cx c ace : contexts * TypedAst.fn option =
  let ce', meta =
    map_acoordinate_element cx
      (rewrite_scheme_fn_inv cx c)
      (fun x -> x)
      (rewrite_scheme_typ cx c) ace in
  check_coordinate_element (with_meta cx meta) c ce'

(* Returns the context with a checked prototype *)
let check_prototype (cx : contexts) ((id, p) : prototype) : contexts =
  debug_print (">> check_prototype " ^ id) ;
  let cx' = bind cx id (Chi (Assoc.empty, None)) in
  with_scheme
    (List.fold_left
       (fun acc (pe, meta) -> check_aprototype_element acc id (pe, meta))
       cx' p)
    Assoc.empty

(* Returns the context with a checked coordinate scheme *)
let check_coordinate (cx : contexts) ((ml, id, p, ce) : coordinate) :
    contexts * TypedAst.fn list =
  debug_print (">> check_coordinate " ^ id) ;
  let pm = get_ml_pm cx ml in
  let cx' = with_scheme (bind cx id (Chi (pm, Some p))) pm in
  let cxr, fl =
    List.fold_left
      (fun (cx', fnl) (ce, meta) ->
        let cx'', tf = check_acoordinate_element cx' id (ce, meta) in
        (cx'', match tf with None -> fnl | Some f -> f :: fnl))
      (cx', []) ce in
  (with_scheme cxr Assoc.empty, fl)

(* Check that there is a void main() defined *)
let check_main_fn (cx : contexts) : unit =
  debug_print ">> check_main_fn" ;
  match get_functions_safe cx "main" with
  | [] -> ()
  | main_fns -> (
      if List.length main_fns > 1 then error cx "Multiple declarations of main"
      else
        let ml, rt, id, pr, meta = snd (List.hd main_fns) in
        let pm = get_ml_pm cx ml in
        debug_print
          ( ">> check_main_fn_2"
          ^ string_of_list string_of_param pr
          ^ string_of_parameterization pm ) ;
        if List.length pr > 0 || Assoc.size pm > 0 then
          error cx "Cannot provide parameters to main"
        else
          match rt with
          | UnitTyp -> ()
          | _ -> raise (TypeException "Expected main function to return void") )

let check_exactCode (ec : string) : TypedAst.prog =
  let test = Assoc.empty in
  [Fn ((ExactCodeTyp, ec, test, []), [])]

let check_structure_member (cx : contexts) ((t, id) : structure_member) :
    TypedAst.structure_member =
  check_typ_valid cx t;
  (typ_erase cx t, id)

let check_structure (cx : contexts) ((name, field_list, pos) : structure) :
    contexts * TypedAst.structure =
  let cx' = bind_structure cx name (name, field_list, pos) in
  let cx'' = bind_tau cx' name true Assoc.empty StructureTyp in
  let _, cx''' = bind_function cx''
    ([], ParTyp(name, []), name,
    List.map (fun (t, id) -> ([], t, id)) field_list, pos) None in
  cx''', (name, List.map (check_structure_member cx) field_list)

let check_class_member (cx : contexts) (mem : class_member) :
    TypedAst.class_member =
  match mem with
  | Field(vis, typ, id) ->
    check_typ_valid cx typ;
    Field(vis, typ_erase cx typ, id)
  | Method(vis, fn) ->
    let _, typed_fn = check_fn cx fn None in
    (* probably not a good idea to just drop the modified contexts like this *)
    match typed_fn with
    | Some tfn -> Method(vis, tfn)
    | None -> debug_fail cx "Typechecking class method returns None"

let check_class (cx : contexts) (c : _class) :
    contexts * TypedAst._class =
  let name, parent, mems, pos = c in
  let cx' = bind_class cx name c in
  let supertype = match parent with
  | Some p -> ParTyp(name, [])
  | None -> ClassTyp in
  let cx'' = bind_tau cx' name true Assoc.empty supertype in
  let constructor_args = match class_method_lookup_shallow c "init" with
  | Some (Method (_, ((_, _, _, p, _), _))) -> p
  | _ -> [] in
  let _, cx''' = bind_function cx''
    ([], ParTyp(name, []), "new_" ^ name, constructor_args, pos) None in
  let typed_mems = List.map (fun m -> check_class_member cx''' m) mems in
  cx''', (name, parent, typed_mems)

let rec check_term (cx : contexts) (t : term) : contexts * TypedAst.prog =
  match t with
  | Using s -> check_exprog (get_prog cx s) cx
  | ExactCode ec -> (cx, check_exactCode ec)
  | Prototype p -> (check_prototype cx p, [])
  | Coordinate c ->
      let cx', tf = check_coordinate cx c in
      (cx', List.map (fun f -> TypedAst.Fn f) tf)
  | Frame f -> (check_frame cx f, [])
  | Typ (ml, id, t) ->
      let pm = get_ml_pm cx ml in
      let ext = has_modification cx ml External in
      (check_typ_decl cx id (ext, pm, t), [])
  | GlobalVar gv -> (
      let cx', gv' = check_global_variable cx gv in
      (cx', match gv' with None -> [] | Some v -> [TypedAst.GlobalVar v]) )
  | Fn f -> (
      let cx', f' = check_fn cx f None in
      (cx', match f' with None -> [] | Some f' -> [Fn f']) )
  | Structure s -> (
      let cx', s' = check_structure cx s in
      (cx', [Structure s']))
  | Class c -> (
      let cx', c' = check_class cx c in
      (cx', [Class c']))
  | Typedef t -> debug_fail cx "Typedef term found in typechecking pass!"

and check_aterm (cx : contexts) ((t, meta) : aterm) : contexts * TypedAst.prog =
  check_term (with_meta cx meta) t

(* This might end up being really bad --
 * there's no scoping management on external files *)
and check_exprog (tl : prog) (cx : contexts) : contexts * TypedAst.prog =
  let cx', f =
    List.fold_left
      (fun acc t ->
        let cx', f' = check_aterm (fst acc) t in
        (cx', f' @ snd acc))
      (cx, []) tl in
  (cx', List.rev f)

let rec check_term_list (tl : prog) (externs : prog Assoc.context) :
    contexts * TypedAst.prog =
  debug_print ">> check_global_var_or_fn_lst" ;
  (* Annoying bootstrapping hack *)
  let cx, f =
    List.fold_left
      (fun acc t ->
        let cx', f' = check_aterm (fst acc) t in
        (cx', (snd acc) @ f'))
      (init (snd (List.hd tl)) externs, [])
      tl in
  (cx, f)

(* Returns the list of fn's which represent the program
 * and params of the void main() fn *)
let check_prog (tl : prog) (externs : prog Assoc.context) : contexts * TypedAst.prog =
  debug_print ">> check_prog" ;
  let cx, typed_prog = check_term_list tl externs in
  check_main_fn cx ;
  debug_print "===================" ;
  debug_print "Type Check Complete" ;
  debug_print "===================\n" ;
  (cx, typed_prog)

(* Searches the program for files which need to be loaded *)
(* If we have any duplicate names, throws an exception to avoid cycles *)
let rec search_prog (p : prog) (found : string list) : string list * string list
    =
  match p with
  | [] -> ([], found)
  | (Using s, meta) :: t ->
      let name =
        String.split_on_char '.'
          (List.hd (List.rev (String.split_on_char '/' s))) in
      if List.length name != 2 then
        error_meta meta "Imported filenames must only have one extension"
      else
        let filename = List.hd name in
        let extension = List.hd (List.tl name) in
        if not (extension = "lgl") then
          error_meta meta ("Extension " ^ extension ^ " not supported")
        else if List.mem filename found then
          error_meta meta ("Duplicate filename " ^ filename ^ " in import chain")
        else
          let tr, found' = search_prog t (filename :: found) in
          (s :: tr, found')
  | _ :: t -> search_prog t found
