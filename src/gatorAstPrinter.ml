(* Tag AST pretty printer *)

open CoreAst
open Util
open GatorAst

let rec string_of_dexp (d : dexp) : string = 
    match d with
    | DimBinop (l, op, r) -> string_of_binop_exp 
        (string_of_dexp l) op (string_of_dexp r)
    | DimNum n -> string_of_int n
    | DimVar s -> s

let rec string_of_typ (t: typ) : string = 
    match t with
    | AutoTyp -> "auto"
    | UnitTyp -> "void"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | ArrTyp (t, d) -> string_of_typ t ^ "[" ^ string_of_list string_of_dexp d ^ "]"
    | VecTyp n -> "vec"^(string_of_int n)
    | ArrLit (t, n) -> string_of_typ t ^ "[" ^ string_of_list string_of_int n ^ "]%lit"
    | VarTyp s -> s
    | CoordTyp (t1, t2) -> string_of_typ t1 ^ "." ^ string_of_typ t2
    | ParTyp (t, tl) -> string_of_typ t ^ "<" ^ (string_of_list string_of_typ tl) ^ ">"
    | TransTyp (t1, t2) -> string_of_typ t1 ^ "->" ^ string_of_typ t2
    | SamplerTyp i -> "sampler" ^ (string_of_int i) ^ "D "
    | SamplerCubeTyp -> "samplerCube"
    | AbsTyp s -> "`" ^ s

let string_of_constraint (c: constrain) : string =
    match c with
    | GenTyp -> "genTyp"
    | GenMatTyp -> "mat"
    | GenVecTyp -> "vec"
    | TypConstraint t -> string_of_typ t
    | AnyTyp -> ""

let string_of_modification (m: modification) : string =
    match m with
    | With l -> string_of_list (fun (x, y) -> 
        "frame(" ^ string_of_int x ^ ") " ^ string_of_list (fun x -> x) y) l
    | Canon -> "canon"

let string_of_mod_list (m: modification list) : string =
    string_of_separated_list " " string_of_modification m

let string_of_param ((ml, t, s): modification list * typ * string) : string =
    string_of_mod_list ml ^ " " ^ (string_of_typ t) ^ " " ^ s

let string_of_parameterization (p : parameterization) : string =
    string_if_true (fun a -> Assoc.size a != 0)
        (fun a -> "<" ^ Assoc.to_string string_of_constraint a ^ ">") p

let string_of_parameterization_decl (pm : parameterization_decl) : string =
    let string_of_pm = string_of_list (fun (s, c) -> s ^ " : " ^ string_of_constraint c) pm in
    string_if_true nonempty (fun p -> "<" ^ string_of_pm ^ ">") pm

let string_of_fn_typ ((p, r, pm, _): fn_typ) : string = 
    (string_of_typ r) ^ string_of_parameterization pm
    ^ "(" ^ string_of_list string_of_param p ^ ")"

let string_of_fn_typ_decl ((pm, r, p): fn_typ_decl) : string = 
    (string_of_typ r) ^ " <" ^ string_of_parameterization_decl pm ^ ">" 
    ^ "(" ^ string_of_list string_of_param p ^ ")"

let string_of_gen_fn_decl (f : 'a -> string) ((fm, t, ft): 'a gen_fn_decl) =
    string_of_mod_list fm ^ f t ^ " " ^ string_of_fn_typ_decl ft

let string_of_fn_decl (fd: fn_decl) : string = string_of_gen_fn_decl (fun x -> x) fd

let rec string_of_aexp ((e, m): aexp) : string =
    string_of_exp e
and string_of_exp (e: exp) : string = 
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> "[" ^ string_of_list string_of_aexp a ^ "]"
    | Unop (op, x) -> (string_of_unop_exp op (string_of_aexp x))
    | Binop (l, op, r) -> 
        let ls = (string_of_aexp l) in
        let rs = (string_of_aexp r) in
        (match op with
        | _ -> (string_of_binop_exp ls op rs))
    | As (e, t) -> (string_of_aexp e) ^ " as " ^ (string_of_typ t)
    | In (e, t) -> (string_of_aexp e) ^ " in " ^ (string_of_typ t)
    | FnInv (i, pr, args) -> i ^ "<" ^ string_of_list string_of_typ pr ^ ">" 
        ^ "(" ^ (string_of_list string_of_aexp args) ^ ")"

let rec string_of_acomm ((c, m) : acomm) : string = 
    string_of_comm c
and string_of_acomm_list (c : acomm list) : string = 
    string_of_separated_list ";\n" string_of_acomm c
and string_of_comm (c: comm) : string =
    let block_string c = "{\n " ^ string_of_acomm_list c ^ "}" in
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (string_of_aexp e) ^ ";"
    | Inc x -> x ^ "++"
    | Dec x -> x ^ "--"
    | Decl (ml, t, s, e) -> (string_of_typ t) ^ " " ^ s ^ " = " ^ (string_of_aexp e) ^ ";"
    | Assign (b, x) -> b ^ " = " ^ (string_of_aexp x) ^ ";"
    | AssignOp (x, op, e) -> x ^ " " ^  string_of_binop op ^ "= " ^ (string_of_aexp e)
    | If ((b, c1), elif_list, c2) -> 
        "if (" ^ string_of_aexp b ^ ")" ^ block_string c1 
        ^ string_of_list (fun (b, c) -> "elif (" ^ string_of_aexp b ^ ")" ^ block_string c) elif_list
        ^ string_of_option_removed (fun x -> "else " ^ block_string x) c2
    | For (d, b, u, cl) -> "for (" ^ string_of_acomm d ^ "; " ^ string_of_aexp b ^ "; " 
        ^ string_of_acomm u ^ ") " ^ block_string cl
    | Return None -> "return;"
    | Return Some e -> "return" ^ (string_of_aexp e) ^ ";"
    | FnCall (n, tl, e) -> string_of_typ n ^ "<" ^ string_of_list string_of_typ tl ^ ">" 
        ^ "(" ^ string_of_list string_of_aexp e ^ ");"

let rec string_of_gen_typ_decl (pred: string) ((s, pmd, t) : typ_decl) : string =
    pred ^ " " ^ s ^ "<" ^ (string_of_parameterization_decl pmd) ^ ">"
    ^ " is " ^ (string_of_typ t) ^ ";"

let string_of_frame_decl (t : typ_decl) = string_of_gen_typ_decl "frame" t
let string_of_typ_decl (t : typ_decl) = string_of_gen_typ_decl "type" t

let string_of_gen_fn (f : 'a -> string) ((g, cl) : 'a gen_fn) : string =
    string_of_gen_fn_decl f g ^ "{" ^  string_of_separated_list "\n" string_of_acomm cl ^"}"

let string_of_fn (fd : fn_decl * acomm list) : string = 
    string_of_gen_fn (fun x -> x) fd

let string_of_prototype_element (pe : prototype_element) : string =
    match pe with
    | ProtoObjectDecl (x, pd) -> "Object " ^ x ^ "<" ^ string_of_parameterization_decl pd ^ ">;"
    | ProtoFnDecl f -> string_of_fn_decl f ^ ";"
    | ProtoBinopDecl f -> string_of_gen_fn_decl string_of_binop f ^ ";"

let string_of_prototype (p : prototype) : string = 
    "prototype {\n" ^ string_of_separated_list "\n" string_of_prototype_element p ^ "}"

let string_of_coordinate_element (ce : coordinate_element) : string = 
    match ce with
    | CoordObjectAssign (x, pd, t) -> x ^ "<" ^ string_of_parameterization_decl pd ^ ">" 
        ^ " = " ^ string_of_typ t ^ ";"
    | CoordFnDecl f -> string_of_fn f
    | CoordBinopDecl f -> string_of_gen_fn string_of_binop f

let string_of_coordinate ((x, n, cl) : coordinate) : string =
    "coordinate " ^ "{\n" ^ "dimension " ^ string_of_int n ^ ";"
    ^ string_of_list (fun ce -> string_of_coordinate_element ce ^ "\n") cl ^ "}"

let string_of_declare (f: fn_decl) : string = 
    "declare " ^ string_of_fn_decl f

let string_of_global_var ((ml, sq, t, x, e) : global_var) : string =
    string_of_mod_list ml ^ " " ^ string_of_storage_qual sq ^ " " ^ string_of_typ t ^ " " ^ x 
    ^ string_of_option_removed (fun x -> "= " ^ string_of_aexp x) e

let string_of_extern (e : extern_decl) : string = 
    match e with
    | ExternFn f -> string_of_declare f
    | ExternVar (m, t, e) -> string_of_mod_list m ^ " " ^ string_of_typ t ^ " " ^ (string_of_aexp e)
    
let string_of_term(t : term) : string = 
    match t with
    | Prototype p -> string_of_prototype p
    | Coordinate c -> string_of_coordinate c
    | FrameDecl f -> string_of_frame_decl f
    | TypDecl t -> string_of_typ_decl t
    | ExternDecl e -> string_of_extern e
    | GlobalVar g -> string_of_global_var g
    | Fn f -> string_of_fn f
let string_of_aterm((t, m) : aterm) : string = 
    string_of_term t

let string_of_prog (tl : prog) : string =
    string_of_separated_list "\n" string_of_aterm tl; 
