(* AST definition of LinGL *)

open CoreAst

type metadata = Lexing.position
type 'a astNode = 'a * metadata

type dexp =
    | DimVar of string
    | DimNum of int
    | DimBinop of dexp * binop * dexp

(* types *)
type typ =
    | AutoTyp
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | ArrTyp of typ * dexp (* i.e. float[5] or bool[2][3] *)
    | VecTyp of int
    | ArrLitTyp of typ * int (* Literals such as [0., 1.] or [true, false] -- constant length *)
    | VarTyp of id (* i.e. color *)
    | CoordTyp of typ * typ (* i.e. cart.point *)
    | ParTyp of typ * typ list (* i.e. point<model> or matrix<model, world> *)
    | SamplerTyp of int
    | SamplerCubeTyp
    | AbsTyp of id (* i.e. `t *)

type constrain =
    (* Special constraint types *)
    | AnyTyp
    | GenTyp
    | GenArrTyp of constrain
    | TypConstraint of typ

(* expressions *)
type aexp = exp astNode
and exp =
    | Val of value
    | Var of string
    | Arr of aexp list
    | Unop of unop * aexp
    | Binop of aexp * binop * aexp
    | As of aexp * typ
    | In of aexp * typ
    | FnInv of string * typ list * args (* function invocation *)
and args = aexp list

type modification =
    | With of (int * (string list)) list
    | Canon

(* function parameterization,
 * which may extend another type. *)
type parameterization = constrain Assoc.context
type parameterization_decl = (string * constrain) list

(* function parameters *)
(* arguments may have an optional parameterization type *)
type params = (modification list * typ * string) list
type ret_typ = typ
(* our functions are not first-order! *)
type fn_typ = params * ret_typ * parameterization * metadata
(* Note that the parameterization declaration is only useful when checking the function, not calling it *)
type fn_typ_decl = parameterization_decl * ret_typ * params
(* General function declarations (includes operation declarations) *)
type 'a gen_fn_decl = modification list * 'a * fn_typ_decl
(* function declaration *)
type fn_decl = string gen_fn_decl

(* commands *)
type acomm = comm astNode
and comm =
    | Skip
    | Print of aexp
    | Inc of id
    | Dec of id
    | Decl of modification list * typ * string * aexp
    | Assign of string * aexp
    | AssignOp of string * binop * aexp
    | If of if_block * if_block list * (acomm list) option  (* if - elif list - else *)
    | For of acomm * aexp * acomm * acomm list
    | Return of aexp option
    | FnCall of typ * typ list * args (* e.g. f<model>(position) -- note that 'f' must be a string, but we treat it as a type to allow parsing of parametrized types *)
and if_block = aexp * acomm list

(* General function declarations *)
type 'a gen_fn = 'a gen_fn_decl * acomm list
(* General function declarations *)
type fn = fn_decl * acomm list

type prototype_element =
    | ProtoObjectDecl of string * parameterization_decl
    | ProtoFnDecl of fn_decl
    | ProtoBinopDecl of binop gen_fn_decl
type prototype = prototype_element list

type coordinate_element =
    | CoordObjectAssign of string * parameterization_decl * typ
    | CoordFnDecl of fn
    | CoordBinopDecl of binop gen_fn
type coordinate = string * int * coordinate_element list

type typ_decl = string * parameterization_decl * typ
type global_var = modification list * storage_qual * typ * string * aexp option
type extern_decl =
    | ExternFn of fn_decl
    | ExternVar of (modification list * typ * aexp)

(* Terms that make up a program *)
(* In any order, we have:
 * Frame Declarations of user types
 * External function declarations without bodies
 * Global variable declarations
 * Function declarations with bodies
 *)
type aterm = term astNode
and term =
    | Prototype of prototype
    | Coordinate of coordinate
    | FrameDecl of typ_decl
    | TypDecl of typ_decl
    | ExternDecl of extern_decl
    | GlobalVar of global_var
    | Fn of fn

(* program *)
type prog = aterm list
