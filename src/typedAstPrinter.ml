(* Tag AST pretty printer *)

open CoreAst
open Util
open TypedAst

let rec string_of_typ (t: etyp) : string = 
  match t with
  | UnitTyp -> "void"
  | BoolTyp -> "bool"
  | IntTyp -> "int"
  | FloatTyp -> "float"
  | StringTyp -> "string"
  | ParTyp (s, tl) -> s ^ string_of_pml tl
  | ArrTyp (t, c) -> string_of_typ t ^ "[" ^ string_of_constvar c ^ "]"
  | AnyTyp -> "any"
  | GenTyp -> "genType"
  | ExactCodeTyp -> "ExactCodeType"
  
and string_of_pml (p : etyp list) : string =
  if List.length p > 0 then "<" ^ string_of_list string_of_typ p ^ ">" else ""

let string_of_parameterization (pm : parameterization) : string =
  if Assoc.size pm > 0 then "<" ^ Assoc.to_string string_of_typ pm  ^ ">" else ""

let rec string_of_texp ((e, _): texp) : string = 
  string_of_exp e
and string_of_exp (e: exp) : string =
  match e with
  | Val v -> string_of_value v
  | Arr a -> "[" ^ string_of_list string_of_texp a ^ "]"
  | Var v -> v
  | Index (l, r) -> 
    string_of_texp l ^ "[" ^ string_of_texp r ^ "]"
      (* id * args *)
  | FnInv (id, tl, args) -> id ^ if nonempty tl then string_of_list string_of_typ tl else ""
    ^ "(" ^ (string_of_list string_of_texp args) ^")"

let string_of_param (i, e) : string = 
    string_of_typ e ^ " " ^ i

let rec string_of_params (p: params) : string =
  string_of_list (fun (x, y) -> string_of_typ x ^ " " ^ y) p

let string_of_global_var ((s, t, i, v) : global_var) =
  (string_of_storage_qual s) ^ " " ^ (string_of_typ t) ^ " " ^ i 
  ^ string_of_option_removed (fun x -> "= " ^ string_of_texp x) v ^ ";"

let string_of_parameterization (pm : parameterization) : string = 
  Assoc.to_string string_of_typ pm

(*Modified*)
let rec string_of_comm (c: comm) : string =
  let block_string c = "{\n " ^ string_of_comm_list c ^ "}" in
  match c with
  | Skip -> "skip;"
  | Print e -> "print " ^ string_of_texp e ^ ";"
  | Exp e -> string_of_texp e ^ ";"
  | Decl (t, s, e) -> string_of_typ t ^ " " ^ s 
    ^ " = " ^ string_of_texp e ^ ";"
  | Assign (b, x) -> string_of_texp b ^ " = " ^ string_of_texp x ^ ";"
  | AssignOp (x, op, e) -> string_of_texp x ^ " " 
    ^ op ^ "= " ^ (string_of_texp e)
  | If ((b, c1), elif_list, c2) -> 
    "if (" ^ string_of_texp b ^ ")" ^ block_string c1 
    ^ string_of_list (fun (b, c) -> "elif (" ^ string_of_texp b ^ ")" ^ block_string c) elif_list
    ^ string_of_option_removed (fun x -> "else " ^ block_string x) c2
  | For (d, b, u, cl) -> "for (" ^ string_of_comm d ^ string_of_texp b ^ "; "
    ^ string_of_comm u ^ ") " ^ block_string cl
  | Return x -> "return" ^ string_of_option_removed (fun x -> " " ^ string_of_texp x) x ^ ";"
  | ExactCodeComm ec -> ec

and string_of_comm_list (cl : comm list) : string = 
   string_of_separated_list "\n" string_of_comm cl

let string_of_fn ((rt,id,pm,p), cl : fn) : string = 
    (string_of_typ rt) ^ " " ^ id ^ string_of_parameterization pm 
    ^ string_of_params p ^ string_of_comm_list cl

let string_of_term (t : term) : string =
  match t with
  | GlobalVar v -> string_of_global_var v
  | Fn f -> string_of_fn f

let string_of_prog (e : prog) : string =
    string_of_separated_list "\n" string_of_term e
