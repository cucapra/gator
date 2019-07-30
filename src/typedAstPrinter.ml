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
    | VecTyp v -> if v = 1 then "float" else "vec" ^ (string_of_int v)
    | MatTyp (m1, m2) -> "mat" ^ (string_of_int m1) ^ "x" ^ (string_of_int m2)
    | TransTyp (s1, s2) -> (string_of_typ s1) ^ "->" ^ (string_of_typ s2)
    | AbsTyp (s, typ) -> "`" ^ s
    | ArrTyp (t, c) -> string_of_typ t ^ "[" ^ string_of_constvar c ^ "]"

let rec string_of_constraint (t: constrain) : string =
    match t with
    | AnyTyp -> "any"
    | GenTyp -> "genType"
    | GenMatTyp -> "mat"
    | GenVecTyp -> "vec"
    | ETypConstraint t -> string_of_typ t

    let string_of_parameterization (p : parameterization) : string =
      string_if_true (fun a -> Assoc.size a > 0)
        (fun a -> "<" ^ Assoc.to_string string_of_constraint a ^ ">") p

let rec string_of_texp ((e, _): texp) : string = 
  string_of_exp e
and string_of_exp (e: exp) : string =
  match e with
  | Val v -> string_of_value v
  | Arr a -> "[" ^ string_of_list string_of_texp a ^ "]"
  | Var v -> v
  | Unop (op, x) -> (string_of_unop_exp op (string_of_texp x))
  | Binop (l, op, r) -> 
      let ls = (string_of_texp l) in
      let rs = (string_of_texp r) in
      (match op with
      | _ -> (string_of_binop_exp ls op rs))
      (* id * args *)
  | FnInv (id, tl, args) -> id ^ string_if_true nonempty (string_of_list string_of_typ) tl
    ^ "(" ^ (string_of_list string_of_texp args) ^")"

let string_of_param (i, e) : string = 
    (string_of_constraint e) ^ " " ^ i

let rec string_of_params (p: params) : string =
  string_of_list (fun (x, y) -> string_of_typ x ^ " " ^ y) p

let string_of_global_var ((s, t, i, v) : global_var) =
  (string_of_storage_qual s) ^ " " ^ (string_of_typ t) ^ " " ^ i 
  ^ string_of_option_removed (fun x -> "= " ^ string_of_texp x) v ^ ";"

let string_of_parameterization (pm : parameterization) : string = 
  Assoc.to_string string_of_constraint pm

let rec string_of_comm (c: comm) : string =
    let block_string c = "{\n " ^ string_of_comm_list c ^ "}" in
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ string_of_texp e ^ ";"
    | Inc (x, _) -> x ^ "++"
    | Dec (x, _) -> x ^ "--"
    | Decl (t, s, e) -> string_of_typ t ^ " " ^ s 
      ^ " = " ^ string_of_texp e ^ ";"
    | Assign (b, x) -> b ^ " = " ^ string_of_texp x ^ ";"
    | AssignOp ((x, _), op, e) -> x ^ " " 
      ^  string_of_binop op ^ "= " ^ (string_of_texp e)
    | If ((b, c1), elif_list, c2) -> 
      "if (" ^ string_of_texp b ^ ")" ^ block_string c1 
      ^ string_of_list (fun (b, c) -> "elif (" ^ string_of_texp b ^ ")" ^ block_string c) elif_list
      ^ string_of_option_removed (fun x -> "else " ^ block_string x) c2
    | For (d, b, u, cl) -> "for (" ^ string_of_comm d ^ string_of_texp b ^ "; " 
      ^ string_of_comm u ^ ") " ^ block_string cl
    | Return x -> "return" ^ string_of_option_removed (fun x -> " " ^ string_of_texp x) x ^ ";"
    | FnCall (id, tl, args) -> id ^ string_if_true nonempty (string_of_bounded_list string_of_typ "<" ">")  tl
      ^ "(" ^ string_of_list string_of_texp args ^")"
 
and string_of_comm_list (cl : comm list) : string = 
   string_of_separated_list "\n" string_of_comm cl

let string_of_fn ((((id, (p, rt, pm)), cl)) : fn) : string = 
    (string_of_typ rt) ^ " " ^ id ^ string_of_parameterization pm 
    ^ string_of_params p ^ string_of_comm_list cl

let string_of_prog (e : prog) : string =
    string_of_separated_list "\n" string_of_fn e
