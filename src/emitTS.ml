open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Util
open EmitUtil
module SS = Set.Make(String)

let rec comp_type (t : etyp) : string =
    match t with
    | UnitTyp -> "void"
    | BoolTyp -> "boolean"
    | IntTyp -> "number"
    | FloatTyp -> "number"
    | VecTyp n -> "vec" ^ (string_of_int n)
    | MatTyp (m, n) -> "mat" ^ (string_of_int (max m n))
    | TransTyp (t1, t2) -> ("Cannot represent TransTyp " ^ comp_type t1 ^ comp_type t2 ^ " in Javascript")
    | AbsTyp (x, _) -> "Cannot represent AbsTyp " ^ x ^ " in Javascript"
    | ArrTyp _ -> failwith "Cannot represent ArrTyp in Javascript"

let comp_fn_arg_type (t : etyp) : string =
    match t with
    | MatTyp (m, n) -> "mat" ^ (string_of_int m) ^ "_" ^ (string_of_int n)
    | _ -> comp_type t

let rec comp_value (v : value) : string =
    match v with
    | Unit -> "null"
    | Bool b -> "<boolean>" ^ string_of_bool b
    | Num n -> "<number>" ^ string_of_int n
    | Float f -> string_of_float f
    | ArrLit a -> string_of_array comp_value a

(* Note the column parameter for padding the matrix size *)
let rec string_of_no_paren_vec (v: exp list) (padding: int) (s : SS.t) : string = 
    (String.concat ", " (List.map (fun e -> comp_exp e s) v)) ^ (repeat ", 0." padding)
  
and string_of_mat (m: exp list list) (s : SS.t) : string = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let max_dim = max r c in
    let string_of_vec_padded = (fun v -> (string_of_no_paren_vec v (max_dim - List.length v) s)) in
    "mat" ^ (string_of_int max_dim) ^ ".fromValues(" ^ (String.concat ", " (List.map string_of_vec_padded tm)) ^
    (repeat (string_of_no_paren_vec [] max_dim s) (max_dim - List.length tm)) ^ ")"

and call_lib_func (t : string) (f : string) (args : exp list) (s : SS.t) : string =
    "__" ^ t ^ f ^ "(" ^ (String.concat "," (List.map (fun e -> comp_exp e s) args)) ^ ")"

and comp_texp ((e, _) : texp) (s : SS.t) : string =
    comp_exp e s
and comp_exp (e : exp) (s : SS.t) : string =
    match e with
    | Val v -> comp_value v
    | Var x -> x
    | Arr a -> (match a with
        | [] -> failwith "cannot have an empty array"
        | (_, t)::_ -> (match t with
            | FloatTyp | IntTyp -> "vec" ^ (string_of_int (List.length a)) ^ ".fromValues(" ^ (String.concat ", " (List.map (fun (e, t) -> comp_exp e s) a)) ^ ")"
            | VecTyp n -> let as_vec_list = (fun v -> (match v with | (Arr a', _) -> (List.map fst a') | _ -> failwith "Typechecker error, a matrix must be a list of vectors")) in
                string_of_mat (List.map as_vec_list a) s
            | _ -> failwith "Typechecker error, every array must be a list of ints, floats, or vectors"))
    | Unop (op, (e, t)) ->
        begin
            match t with
            | VecTyp n ->
                begin
                    match op with
                    | Neg -> call_lib_func ("vec" ^ (string_of_int n)) "negate" [e] s
                    | _ -> failwith "Cannot apply this operator to vector"
                end
            | MatTyp (m, n) ->
                begin
                    match op with
                    | Neg -> comp_exp (Binop ((Val (Num (-1)), IntTyp), Times, (e,t))) s
                    | _ -> failwith "Cannot apply this operator to matrix"
                end
            | _ -> string_of_unop_exp op (comp_exp e s)
        end
    | Binop ((e1, t1), op, (e2, t2)) ->
        begin
            let vec_and_vec (typ_string : string) : string =
                if op = Eq then typ_string ^ ".equals(" ^ (comp_exp e1 s) ^ "," ^ (comp_exp e2 s) ^ ")"
                    else
                        let op_string = match op with
                            | Plus -> "add"
                            | Minus -> "sub"
                            | Div -> "div"
                            | CTimes  -> "mul"
                            | _ -> failwith ("Cannot apply " ^ (string_of_binop op) ^ " to vectors")
                        in
                        call_lib_func typ_string op_string [e1;e2] s
            in
            let vec_and_num (typ_string : string) (vec_exp : exp) ((scalar_exp, scalar_typ) : texp) : string =
                let scalar = match op with
                    | Times -> scalar_exp
                    | Div -> Binop (((Val (Num 1)), IntTyp), Div, (scalar_exp, scalar_typ))
                    | _ -> failwith ("Cannot apply " ^ (string_of_binop op) ^ " to vector and scalar")
                in
                call_lib_func typ_string "scale" [vec_exp;scalar] s
            in
            match t1, t2 with
            | VecTyp n, VecTyp _ ->
                vec_and_vec ("vec" ^ (string_of_int n))
            | IntTyp, VecTyp n | FloatTyp, VecTyp n ->
                vec_and_num ("vec" ^ (string_of_int n)) e2 (e1, t1)
            | VecTyp n, IntTyp | VecTyp n, FloatTyp ->
                vec_and_num ("vec" ^ (string_of_int n)) e1 (e2, t2)
            | MatTyp (ldim, _), VecTyp rdim ->
                if ldim = rdim then call_lib_func ("vec" ^ (string_of_int rdim)) ("transformMat" ^ (string_of_int rdim)) [e2;e1] s
                else if ldim > rdim then "__vec" ^ (string_of_int ldim) ^ "transformMat" ^ (string_of_int ldim) ^ "(__vec" ^ (string_of_int rdim) ^ "to" ^ (string_of_int ldim) ^ "(" ^  (comp_exp e2 s) ^ ")," ^ (comp_exp e1 s) ^ ")"
                else "__vec" ^ (string_of_int rdim) ^ "to" ^ (string_of_int ldim) ^ "(" ^ (call_lib_func ("vec" ^ (string_of_int rdim)) ("transformMat" ^ (string_of_int rdim)) [e2;e1] s) ^ ")"
            | IntTyp, MatTyp (m,n) | FloatTyp, MatTyp (m,n) ->
                call_lib_func ("mat" ^ (string_of_int (max m n))) "multiplyScalar" [e2;e1] s
            | MatTyp (m,n), IntTyp | MatTyp (m,n), FloatTyp ->
                call_lib_func ("mat" ^ (string_of_int (max m n))) "multiplyScalar" [e1;e2] s
            | MatTyp (ldim, idim), MatTyp (_, rdim) ->
                let typ_string = "mat" ^ (string_of_int (max (max ldim rdim) idim)) in
                begin
                    match op with
                    | Plus -> call_lib_func typ_string "add" [e1;e2] s
                    | Minus -> call_lib_func typ_string "sub" [e1;e2] s
                    | Div -> call_lib_func typ_string "div" [e1;e2] s
                    | Eq -> typ_string ^ ".equals(" ^ (comp_exp e1 s) ^ "," ^ (comp_exp e2 s) ^ ")"
                    | Times ->
                        if ldim = rdim && ldim >= idim then call_lib_func typ_string "mul" [e1;e2] s
                        else if idim = rdim && idim >= ldim then call_lib_func typ_string "mul" [e1;e2] s
                        else if ldim = rdim then "__" ^ typ_string ^ "to" ^ (string_of_int ldim) ^ "(" ^ (call_lib_func typ_string "mul" [e1;e2] s) ^ ")"
                        else if idim >= ldim && idim >= rdim then "__" ^ typ_string ^ "to" ^ (string_of_int (max ldim rdim)) ^ "(" ^ (call_lib_func typ_string "mul" [e1;e2] s) ^ ")"
                        else if ldim > rdim then "__" ^ typ_string ^ "mul(" ^ (comp_exp e1 s) ^ ",__mat" ^ (string_of_int (max idim rdim)) ^ "to" ^ (string_of_int ldim) ^ "(" ^ (comp_exp e2 s) ^ "))"
                        else if rdim > ldim then "__" ^ typ_string ^ "mul(" ^ "__mat" ^ (string_of_int (max idim ldim)) ^ "to" ^ (string_of_int rdim) ^ "(" ^ (comp_exp e1 s) ^ ")," ^ (comp_exp e2 s) ^ ")"
                        else failwith "Impossible condition"
                    | _ -> failwith ("Cannot apply " ^ (string_of_binop op) ^ " to matrices")
                end
            | _ -> "(" ^ string_of_binop_exp (comp_exp e1 s) op (comp_exp e2 s) ^ ")"
        end
    | FnInv (f, tpl, args) ->
        let fn_name =
            if SS.mem f s then f ^ "__" ^ (String.concat "__" (List.map comp_fn_arg_type tpl))
            else (String.concat "__" (List.map comp_fn_arg_type tpl)) ^ "." ^ f
        in
        fn_name ^ "(" ^ (String.concat "," (List.map (fun (e, _) -> comp_exp e s) args)) ^ ")"

let comp_assign (x : id) ((e, t) : texp) (s : SS.t) : string =
    match t with
    | UnitTyp | BoolTyp | IntTyp | FloatTyp | AbsTyp _ -> x ^ "=" ^ (comp_exp e s) ^ ";"
    | VecTyp v -> "vec" ^ (string_of_int v) ^ ".copy(" ^ x ^ "," ^ (comp_exp e s) ^ ");"
    | MatTyp (m, n) -> "mat" ^ (string_of_int (max m n)) ^ ".copy(" ^ x ^ "," ^ (comp_exp e s) ^ ");"
    | TransTyp _ | ArrTyp _ -> comp_type t

let rec comp_comm_lst (cl : comm list) (s : SS.t) : string =
    debug_print ">> comp_comm_lst";
    match cl with
    | [] -> ""
    | h::tl -> match h with
        | Skip -> comp_comm_lst tl s
        | Print (e, _) -> "console.log(" ^ (comp_exp e s) ^ ");" ^ comp_comm_lst tl s
        | Inc (x, _) -> x ^ "++;" ^ (comp_comm_lst tl s)
        | Dec (x, _) -> x ^ "--;" ^ (comp_comm_lst tl s)
        | Decl (et, x, e) ->
            let create_str = match et with
                | VecTyp n -> "let " ^ x ^ "=vec" ^ (string_of_int n) ^ ".create();"
                | MatTyp (m, n) -> "let " ^ x ^ "=mat" ^ (string_of_int (max m n)) ^ ".create();"
                | _ -> "let "
            in
            create_str ^ (comp_assign x e s) ^ (comp_comm_lst tl s)
        | Assign (x, e) -> (comp_assign x e s) ^ (comp_comm_lst tl s)
        | AssignOp ((x, t), op, e) -> (comp_assign x ((Binop ((Var x, t), op, e)), t) s) ^ (comp_comm_lst tl s)
        | If (((b, _), c1), el, c2) -> 
            ("if " ^ "(" ^ (comp_exp b s) ^ ")"
            ^ "{" ^ (comp_comm_lst c1 s) ^ "}"
            ^ (List.fold_left (fun acc ((b, _), c) -> "if " ^ "(" ^ (comp_exp b s) ^ ")"
                ^ "{" ^ (comp_comm_lst c s) ^ "}" ^ acc) "" el)
            ^ (match c2 with | Some c2 -> " else {" ^ (comp_comm_lst c2 s) ^ "}" | None -> "")
            ^ (comp_comm_lst tl s))
        | For (i, (cond, _), after, cl) ->
            ("for (" ^ (comp_comm_lst [i] s) ^ " " ^ (comp_exp cond s) ^ "; " ^ (comp_comm_lst [after] s |> (String.split_on_char ';') |> List.hd) ^ ")"
            ^ "{ " ^ (comp_comm_lst cl s) ^ " }" ^ (comp_comm_lst tl s))
        | Return Some (e, _) -> "return " ^ (comp_exp e s) ^ ";" ^ (comp_comm_lst tl s)
        | Return None -> "return;" ^ (comp_comm_lst tl s)
        | FnCall (f, tpl, args) ->
            let fn_name =
                if SS.mem f s then f ^ "__" ^ (String.concat "__" (List.map comp_fn_arg_type tpl))
                else (String.concat "__" (List.map comp_fn_arg_type tpl)) ^ "." ^ f
            in
            fn_name ^ "(" ^ (String.concat "," (List.map (fun (e, _) -> comp_exp e s) args)) ^ ");" ^ (comp_comm_lst tl s)

let comp_fn (f : fn) (s : SS.t) : string =
    let ((id, (p, rt, pm)), cl) = f in
    debug_print (">> comp_fn" ^ id);
    let param_string = string_of_list (fun (t, i) -> i ^ ":" ^ comp_type t) p in
    let fn_name = id ^ "__" ^ (String.concat "__" (List.map (fun (_, c) -> 
        match c with ETypConstraint t -> comp_fn_arg_type t 
        | _ -> failwith ("Function cannot have resolved type parameter " ^ string_of_constraint c)) (Assoc.bindings pm))) in
    let fn_str = "function " ^ fn_name ^ "(" ^ param_string ^ "):" ^ (comp_type rt) ^ "{" ^ (comp_comm_lst cl s) ^ "}" in
    fn_str

let rec comp_fn_lst (f : fn list) (s : SS.t) : string =
    debug_print ">> comp_fn_lst";
    match f with 
    | [] -> ""
    | ((x, fd), cl)::t -> (comp_fn ((x, fd), cl) s) ^ (comp_fn_lst t (SS.add x s))

let rec decl_attribs (gv : global_vars) : string = 
    debug_print ">> decl_attribs";
    match gv with
    | [] -> ""
    | (sq, et, x, e)::t -> let e_str = string_of_option_removed (fun x -> "= " ^ comp_texp x SS.empty) e in
        match et with
        | VecTyp n -> "var " ^ x ^ "= vec" ^ (string_of_int n) ^ ".create();" ^ x ^ e_str ^ (decl_attribs t)
        | MatTyp (m,n) -> "var " ^ x ^ "= mat" ^ (string_of_int (max m n)) ^ ".create();" ^ e_str ^ (decl_attribs t)
        | _ -> "var " ^ x ^ e_str ^ ";" ^ (decl_attribs t)

let util_funcs =
    String.concat "" (List.map
        (fun (t, f, args) ->
            "function __" ^ t ^ f ^ "(" ^ args ^ "):" ^ t ^ "{var out=" ^ t ^ ".create();" ^ t ^ "." ^ f ^ "(out," ^ args ^ ");return out;}")
        [
            ("vec2", "add", "a,b");
            ("vec2", "sub", "a,b");
            ("vec2", "mul", "a,b");
            ("vec2", "negate", "a");
            ("vec2", "scale", "a,b");
            ("vec2", "transformMat2", "a,m");
            ("vec3", "add", "a,b");
            ("vec3", "sub", "a,b");
            ("vec3", "mul", "a,b");
            ("vec3", "negate", "a");
            ("vec3", "scale", "a,b");
            ("vec3", "transformMat3", "a,m");
            ("vec4", "add", "a,b");
            ("vec4", "sub", "a,b");
            ("vec4", "mul", "a,b");
            ("vec4", "negate", "a");
            ("vec4", "scale", "a,b");
            ("vec4", "transformMat4", "a,m");
            ("mat2", "add", "a,b");
            ("mat2", "sub", "a,b");
            ("mat2", "mul", "a,b");
            ("mat2", "multiplyScalar", "a,b");
            ("mat3", "add", "a,b");
            ("mat3", "sub", "a,b");
            ("mat3", "mul", "a,b");
            ("mat3", "multiplyScalar", "a,b");
            ("mat4", "add", "a,b");
            ("mat4", "sub", "a,b");
            ("mat4", "mul", "a,b");
            ("mat4", "multiplyScalar", "a,b");
        ]) ^
    "function __vec2to2(v:vec2):vec2{return v;}" ^
    "function __vec2to3(v:vec2):vec3{return vec3.fromValues(v[0],v[1],0);}" ^
    "function __vec2to4(v:vec2):vec4{return vec4.fromValues(v[0],v[1],0,0);}" ^
    "function __vec3to3(v:vec3):vec3{return v;}" ^
    "function __vec3to4(v:vec3):vec4{return vec4.fromValues(v[0],v[1],v[2],0);}" ^
    "function __vec4to4(v:vec4):vec4{return v;}" ^
    "function __vec4to3(v:vec4):vec3{return vec3.fromValues(v[0],v[1],v[2]);}" ^
    "function __vec4to2(v:vec4):vec2{return vec2.fromValues(v[0],v[1]);}" ^
    "function __vec3to2(v:vec3):vec2{return vec2.fromValues(v[0],v[1]);}" ^
    "function __mat2to3(m:mat2):mat3{return mat3.fromValues(m[0],m[1],0,m[2],m[3],0,0,0,0);}" ^
    "function __mat2to4(m:mat2):mat4{return mat4.fromValues(m[0],m[1],0,0,m[2],m[3],0,0,0,0,0,0,0,0,0,0);}" ^
    "function __mat3to4(m:mat3):mat4{return mat4.fromValues(m[0],m[1],m[2],0,m[3],m[4],m[5],0,m[6],m[7],m[8],0,0,0,0,0);}" ^
    "function __mat4to3(m:mat4):mat3{return mat3.fromValues(m[0],m[1],m[2],m[4],m[5],m[6],m[8],m[9],m[10]);}" ^
    "function __mat4to2(m:mat4):mat2{return mat2.fromValues(m[0],m[1],m[4],m[5]);}" ^
    "function __mat3to2(m:mat3):mat2{return mat2.fromValues(m[0],m[1],m[3],m[4]);}"

let rec compile_program (prog : prog) (global_vars : global_vars) : string =
    debug_print ">> compile_programJS";
    let prog' = generate_generics_in_prog prog false in
    "import {vec2,mat2,vec3,mat3,vec4,mat4} from 'gl-matrix';" ^ util_funcs ^ "\n" ^ (decl_attribs global_vars) ^ (comp_fn_lst prog' SS.empty) ^ "main__();"