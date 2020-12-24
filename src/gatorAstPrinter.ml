(* Tag AST pretty printer *)

open CoreAst
open Util
open GatorAst

let string_of_decl (x : string) f : string = f x

let rec string_of_dexp (d : dexp) : string =
  match d with
  | DimPlus (l, r) -> string_of_dexp l ^ " + " ^ string_of_dexp r
  | DimNum n -> string_of_int n
  | DimVar s -> s

(* let rec string_of_dtyp (d : dtyp) : string =
    match d with
    | BaseTyp s -> s
    | MultTyp (x, y) -> string_of_dtyp x ^ " * " ^ string_of_dtyp y *)

let rec string_of_typ (t : typ) : string =
  match t with
  | BotTyp -> "bottyp"
  | AutoTyp -> "auto"
  | UnitTyp -> "void"
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FrameTyp d -> "frame<" ^ string_of_dexp d ^ ">"
  | FloatTyp -> "float"
  | StringTyp -> "string"
  (* | DimTyp d -> "dimension" ^ string_of_dtyp(d) *)
  | ThisTyp -> "this"
  | Literal t -> "%(" ^ string_of_typ t ^ ")"
  | ArrTyp (t, d) -> string_of_typ t ^ "[" ^ string_of_dexp d ^ "]"
  (* Essentially the bottom type for all arrays *)
  | MemberTyp (c, t) -> string_of_typ c ^ "." ^ string_of_typ t
  | ParTyp (s, tl) -> s ^ string_of_pml tl
  | GenTyp -> "genType"
  | GenArrTyp t' -> "arr of " ^ string_of_typ t'
  | AnyFrameTyp -> "frame"
  | AnyTyp -> "anyType"
  | ExactCodeTyp -> "ExactCodeTyp"
  | StructureTyp -> "struct"

and string_of_pml (p : typ list) : string =
  if List.length p > 0 then "<" ^ string_of_list string_of_typ p ^ ">" else ""

let string_of_modification (m : modification) : string =
  match m with
  | With (t, pm, b) ->
      if b then
        "with " ^ string_of_list (fun x -> x) pm ^ " < " ^ string_of_typ t ^ ":"
      else
        "with " ^ string_of_typ t ^ " " ^ string_of_list (fun x -> x) pm ^ ":"
  | Canon -> "canon"
  | External -> "declare"
  | Storage_Qualifier sq -> string_of_storage_qual sq

let string_of_mod_list (m : modification list) : string =
  string_of_separated_list " " string_of_modification m
  ^ if List.length m > 0 then " " else ""

let string_of_param ((ml, t, s) : modification list * typ * string) : string =
  string_of_mod_list ml ^ string_of_typ t ^ " " ^ s

let string_of_parameterization (pm : parameterization) : string =
  if Assoc.size pm != 0 then
    "<"
    ^ List.fold_left
        (fun acc s -> string_of_typ s ^ acc)
        ""
        (List.map fst (Assoc.values pm))
    ^ ">"
  else ""

let string_of_fn_typ ((ml, r, x, p, _) : fn_typ) : string =
  string_of_mod_list ml ^ string_of_typ r ^ " " ^ x ^ "("
  ^ string_of_list string_of_param p
  ^ ")"

let rec string_of_aexp ((e, m) : aexp) : string = string_of_exp e

and string_of_exp (e : exp) : string =
  match e with
  | Val v -> string_of_value v
  | Var v -> v
  | Arr a -> "[" ^ string_of_list string_of_aexp a ^ "]"
  | Index (a, i) -> string_of_aexp a ^ "[" ^ string_of_aexp i ^ "]"
  | As (e, t) -> string_of_aexp e ^ " as " ^ string_of_typ t
  | In (e, t) -> string_of_aexp e ^ " in " ^ string_of_typ t
  | FnInv (i, pr, args) ->
      i ^ string_of_pml pr ^ "(" ^ string_of_list string_of_aexp args ^ ")"
  | FieldSelect (Some e, s) -> string_of_exp e ^ "." ^ s
  | FieldSelect (None, s) -> "this." ^ s

let rec string_of_acomm ((c, m) : acomm) : string = string_of_comm c

and string_of_acomm_list (c : acomm list) : string =
  string_of_separated_list ";\n" string_of_acomm c

and string_of_comm (c : comm) : string =
  let block_string c = "{\n " ^ string_of_acomm_list c ^ "}" in
  match c with
  | Skip -> "skip;"
  | Print e -> "print " ^ string_of_aexp e ^ ";"
  | Exp e -> string_of_aexp e ^ ";"
  | Decl (ml, t, s, e) ->
      string_of_mod_list ml ^ string_of_typ t ^ " " ^ s ^ " = "
      ^ string_of_aexp e ^ ";"
  | Assign (b, x) -> string_of_aexp b ^ " = " ^ string_of_aexp x ^ ";"
  | AssignOp (x, op, e) -> string_of_aexp x ^ " " ^ op ^ "= " ^ string_of_aexp e
  | If ((b, c1), elif_list, c2) ->
      "if (" ^ string_of_aexp b ^ ")" ^ block_string c1
      ^ string_of_list
          (fun (b, c) -> "elif (" ^ string_of_aexp b ^ ")" ^ block_string c)
          elif_list
      ^ Option.fold c2 ~some:(fun x -> "else " ^ block_string x) ~none:""
  | For (d, b, u, cl) ->
      "for (" ^ string_of_acomm d ^ "; " ^ string_of_aexp b ^ "; "
      ^ string_of_acomm u ^ ") " ^ block_string cl
  | Return None -> "return;"
  | Return (Some e) -> "return " ^ string_of_aexp e ^ ";"
  | ExactCodeComm ec -> ec

let string_of_frame ((x, d) : frame) =
  "frame " ^ x ^ " is " ^ string_of_dexp d ^ ";"

let string_of_typedef ((t, x) : typedef) : string =
    "typedef " ^ (string_of_typ t) ^ " " ^ x ^ ";"

let string_of_fn ((t, c) : fn_typ * acomm list) : string =
  string_of_fn_typ t ^ "{\n"
  ^ string_of_separated_list "\n" string_of_acomm c
  ^ "}"

let string_of_prototype_element (pe : prototype_element) : string =
  match pe with
  | ProtoObject (ml, x, t) ->
      string_of_mod_list ml ^ "Object " ^ x
      ^ Option.fold ~none:"" ~some:(fun t' -> " is " ^ string_of_typ t') t
  | ProtoFn f -> string_of_fn_typ f ^ ";"

let string_of_prototype ((x, p) : prototype) : string =
  "prototype " ^ x ^ "{\n"
  ^ string_of_separated_list "\n" string_of_prototype_element (List.map fst p)
  ^ "}"

let string_of_coordinate_element (ce : coordinate_element) : string =
  match ce with
  | CoordObjectAssign (ml, x, t) ->
      string_of_mod_list ml ^ x ^ " is " ^ string_of_typ t ^ ";"
  | CoordFn f -> string_of_fn f

let string_of_coordinate ((ml, x, p, cl) : coordinate) : string =
  string_of_mod_list ml ^ "coordinate " ^ x ^ " : " ^ p ^ "{\n"
  ^ string_of_list
      (fun ce -> string_of_coordinate_element ce ^ "\n")
      (List.map fst cl)
  ^ "}"

let string_of_global_var ((ml, t, x, e) : global_var) : string =
  string_of_mod_list ml ^ " " ^ string_of_typ t
  ^ " " ^ x
  ^ Option.fold ~none:"" ~some:(fun x -> "= " ^ string_of_aexp x) e

let string_of_structure (id, ml, _) : string =
  (List.fold_left
  (fun s (t, id) -> 
    s ^ " " ^ string_of_typ t ^ " " ^ id ^ ";"
  ) ("struct " ^ id ^ " {") ml) ^ " }"

let string_of_term (t : term) : string =
  match t with
  | Using s -> "using " ^ s
  | ExactCode s -> "exact code: " ^ s
  | Prototype p -> string_of_prototype p
  | Coordinate c -> string_of_coordinate c
  | Frame f -> string_of_frame f
  | Typ (ml, x, t) ->
      string_of_mod_list ml ^ "type " ^ x ^ " is " ^ string_of_typ t
  | GlobalVar g -> string_of_global_var g
  | Fn f -> string_of_fn f
  | Structure s -> string_of_structure s
  | Typedef t -> string_of_typedef t

let string_of_aterm ((t, _) : aterm) : string = string_of_term t

let string_of_prog (tl : prog) : string =
  string_of_separated_list "\n" string_of_aterm tl
