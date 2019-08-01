(* AST definition of LinGL *)

open CoreAst

type metadata = Lexing.position
type 'a astNode = 'a * metadata

type 'a decl = string * 'a

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
    | ArrLitTyp of typ * int (* Literals such as [0., 1.] or [true, false] -- constant length *)
    | ArrTyp of typ * dexp (* i.e. float[5] or bool[2][3] *)
    | CoordTyp of string * typ (* i.e. cart.point *)
    | ParTyp of string * typ list (* i.e. color or matrix<model, world> *)

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

(* function and type parameterization,
 * which may extend another type. *)
type parameterization = constrain Assoc.context

(* function parameters *)
(* arguments may have an optional parameterization type *)
type params = (typ * string) list
type ret_typ = typ
(* our functions are not first-order! *)
type fn_typ = modification list * parameterization * params * ret_typ * metadata
(* General function declarations (includes operation declarations) *)
type 'a gen_fn_decl = modification list * 'a * fn_typ

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
    | If of if_block * if_block list * acomm list option  (* if - elif list - else *)
    | For of acomm * aexp * acomm * acomm list
    | Return of aexp option
    | FnCall of string * typ list * args (* Function invocation as a command *)
    (* e.g. f<model>(position) -- note that 'f' must be a string, 
    but we treat it as a type to allow parsing of parametrized types *)
and if_block = aexp * acomm list

(* General function declarations *)
type 'a gen_fn = 'a gen_fn_decl * acomm list
(* General function declarations *)
type fn = string gen_fn

type frame =
    | FrameDim of string
    | FrameNum of int

type partyp = parameterization * typ

type prototype_element =
    | ProtoObjectDecl of string * parameterization
    | ProtoFnDecl of fn_typ
    | ProtoBinopDecl of binop gen_fn_decl
(* Name and list of declarations *)
type prototype = string * (string * prototype_element) list

type coordinate_element =
    | CoordObjectAssign of parameterization * typ
    | CoordFnDecl of fn_typ
    | CoordBinopDecl of binop gen_fn
(* Name, underlying prototype, dimension, and list of definitions *)
type coordinate = string * string * int * (string * coordinate_element) list

type global_var = modification list * storage_qual * typ * string * aexp option
type extern_element =
    | ExternFn of fn_typ
    | ExternVar of modification list * typ * aexp

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
    | FrameDecl of frame decl
    | TypDecl of string * parameterization * typ
    | ExternDecl of extern_element
    | GlobalVar of global_var
    | Fn of fn

(* program *)
type prog = aterm list
