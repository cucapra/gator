open CoreAst
open TypedAst
open TypedAstPrinter
open Util

exception EmitException of string

let error s = raise (EmitException s)

(* Used in emitters for recovering infix notation of member function names *)
let unop_list = ["-"]

let binop_list =
  [ "+"; "-"; "*"; "/"; "=="; "&&"; "||"; ">="; "<="; ">"; "<"; ".*"; "&"; "|"
  ; "^" ]

let replace_list =
  [ (Str.regexp "+", "_gator_plus"); (Str.regexp "-", "_gator_minus")
  ; (Str.regexp "*", "_gator_times") ]

let replace_all_in_name id =
  List.fold_left (fun acc (r, x) -> Str.global_replace r x acc) id replace_list

let string_of_fn_util (id : string) (args : string list) : string =
  let base =
    replace_all_in_name id ^ "(" ^ string_of_list (fun x -> x) args ^ ")" in
  match args with
  | [a] -> if List.mem id unop_list then id ^ a else base
  | [a; b] -> if List.mem id binop_list then "(" ^ a ^ id ^ b ^ ")" else base
  | _ -> base

let rec replace_type (et : etyp) (t : etyp) (r : etyp) : etyp =
  let rec eq (et : etyp) (t : etyp) : bool =
    match (et, t) with
    | UnitTyp, UnitTyp
     |BoolTyp, BoolTyp
     |IntTyp, IntTyp
     |FloatTyp, FloatTyp
     |StringTyp, StringTyp ->
        true
    (* | VecTyp m, VecTyp n -> m == n
       | MatTyp(k, l), MatTyp(m, n) -> k == m && l == n
       | TransTyp(et1, et2), TransTyp(et3, et4) -> eq et1 et3 && eq et2 et4
       | AbsTyp(x, _), AbsTyp(y, _) -> debug_print (x ^ " " ^ y); debug_print (string_of_bool (x = y)); x = y *)
    | _, _ -> false in
  if eq et t then r
  else
    match et with
    (* | TransTyp(t1, t2) -> TransTyp(replace_type t1 t r, replace_type t2 t r) *)
    | ArrTyp (t', c) -> ArrTyp (replace_type t' t r, c)
    | _ -> et

let rec replace_type_in_texp ((e, et) : texp) (t : etyp) (r : etyp) : texp =
  let e' =
    match e with
    | Arr a -> Arr (replace_type_in_args a t r)
    | Index (l, i) ->
        Index (replace_type_in_texp l t r, replace_type_in_texp i t r)
    | FnInv (x, tl, a) ->
        FnInv
          ( x
          , List.map (fun pt -> replace_type pt t r) tl
          , replace_type_in_args a t r )
    | _ -> e in
  let et' = replace_type et t r in
  (e', et')

and replace_type_in_args (a : args) (t : etyp) (r : etyp) : args =
  match a with
  | [] -> []
  | te :: tl -> replace_type_in_texp te t r :: replace_type_in_args tl t r

let rec replace_type_in_comm (c : comm) (t : etyp) (r : etyp) : comm =
  match c with
  | Print te -> Print (replace_type_in_texp te t r)
  | Decl (et, x, te) ->
      Decl (replace_type et t r, x, replace_type_in_texp te t r)
  | Assign (x, te) -> Assign (x, replace_type_in_texp te t r)
  | AssignOp ((x, et), o, te) ->
      AssignOp ((x, replace_type et t r), o, replace_type_in_texp te t r)
  | If ((te, cl), ibl, clo) ->
      let rec replace_type_in_if_block_list (ibl : if_block list) :
          if_block list =
        match ibl with
        | [] -> []
        | (te, cl) :: tl ->
            (replace_type_in_texp te t r, replace_type_in_comm_list cl t r)
            :: replace_type_in_if_block_list tl in
      let clo' =
        match clo with
        | None -> None
        | Some cl -> Some (replace_type_in_comm_list cl t r) in
      If
        ( (replace_type_in_texp te t r, replace_type_in_comm_list cl t r)
        , replace_type_in_if_block_list ibl
        , clo' )
  | For (c1, te, c2, cl) ->
      For
        ( replace_type_in_comm c1 t r
        , replace_type_in_texp te t r
        , replace_type_in_comm c2 t r
        , replace_type_in_comm_list cl t r )
  | Return teo -> (
    match teo with
    | None -> Return None
    | Some te -> Return (Some (replace_type_in_texp te t r)) )
  | _ -> c

and replace_type_in_comm_list (cl : comm list) (t : etyp) (r : etyp) : comm list
    =
  match cl with
  | [] -> []
  | c :: tl -> replace_type_in_comm c t r :: replace_type_in_comm_list tl t r

let rec replace_type_in_params (p : params) (t : etyp) (r : etyp) : params =
  match p with
  | [] -> []
  | (sq, et, x) :: tl -> (sq, replace_type et t r, x) :: replace_type_in_params tl t r

let rec replace_type_in_parameterization (pm : parameterization) (t : etyp)
    (r : etyp) : parameterization =
  let pm_list = Assoc.bindings pm in
  Assoc.create
    (List.map
       (fun (n, c) ->
         match t with
         | ParTyp (x, _) -> if x = n then (x, r) else (n, c)
         | _ -> (n, c))
       pm_list)

let rec replace_type_in_fn (((rt, n, pm, p), cl) : fn) (t : etyp) (r : etyp) :
    fn =
  let p' = replace_type_in_params p t r in
  let rt' = replace_type rt t r in
  let cl' = replace_type_in_comm_list cl t r in
  let pm' = replace_type_in_parameterization pm t r in
  ((rt', n, pm', p'), cl')

let rec replace_type_in_fn_list (fl : fn list) (t : etyp) (r : etyp) : fn list =
  match fl with
  | [] -> []
  | f :: tl -> replace_type_in_fn f t r :: replace_type_in_fn_list tl t r

let rec list_of_constraint (t : etyp) (int_and_float : bool) : etyp list =
  match t with
  | AnyTyp -> BoolTyp :: list_of_constraint GenTyp int_and_float
  | GenTyp -> if int_and_float then [IntTyp; FloatTyp] else [IntTyp]
  | _ -> [t]

let generate_generics_for_fn (((rt, s, pm, p), cl) : fn) (int_and_float : bool)
    : fn list =
  let pm_list = List.rev (Assoc.bindings pm) in
  let rec replace_generic (orig : fn list) (pml : (string * etyp) list) :
      fn list =
    match pml with
    | [] -> orig
    | (s, c) :: tl ->
        let lst = list_of_constraint c int_and_float in
        let result =
          List.fold_left
            (fun acc t ->
              debug_print (string_of_typ t) ;
              replace_type_in_fn_list orig (ParTyp (s, [c])) t @ acc)
            [] lst in
        replace_generic result tl in
  replace_generic [((rt, s, pm, p), cl)] pm_list

(* let rec generate_generics_in_prog (p : fn list) (int_and_float : bool) : prog =
    let p' = match p with
        | [] -> []
        | f :: tl -> generate_generics_for_fn f int_and_float @ generate_generics_in_prog tl int_and_float
    in
    let l = [2; 3; 4] in
    List.fold_left (fun a m -> (List.fold_left (fun a n -> replace_type_in_fn_list a (TransTyp(VecTyp n, VecTyp m)) (MatTyp(m,n))) a l)) p' l *)
