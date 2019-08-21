(* AST definition of LinGL *)

open CoreAst

type metadata = Lexing.position
type 'a astNode = 'a * metadata

type dexp =
    | DimVar of string
    | DimNum of int
    | DimPlus of dexp * dexp

(* types *)
type typ =
    | BotTyp (* Useful exactly for empty lists *)
    | AutoTyp
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | Literal of typ
    | ArrTyp of typ * dexp (* i.e. float[5] or bool[2][3] *)
    | CoordTyp of string * typ (* i.e. cart.point *)
    | ParTyp of string * typ list (* i.e. color or matrix<model, world> *)
    | AnyTyp
    | GenTyp
    | GenArrTyp of typ

(* expressions *)
type aexp = exp astNode
and exp =
    | Val of value
    | Var of string
    | Arr of aexp list
    | Index of aexp * aexp (* a[3] -- special operation that can't be user-defined *)
    | As of aexp * typ
    | In of aexp * typ
    | FnInv of string * typ list * args (* function invocation *)
and args = aexp list

type modification =
    | With of (int * (string list)) list
    | Canon

(* commands *)
type acomm = comm astNode
and comm =
    | Skip
    | Print of aexp
    | Inc of id
    | Dec of id
    | Decl of modification list * typ * string * aexp 
    | Assign of string * aexp
    | AssignOp of string * string * aexp
    | If of if_block * if_block list * acomm list option  (* if - elif list - else *)
    | For of acomm * aexp * acomm * acomm list
    | Return of aexp option
    | FnCall of string * typ list * args (* Function invocation as a command *)
    (* e.g. f<model>(position) -- note that 'f' must be a string, 
    but we treat it as a type to allow parsing of parametrized types *)
and if_block = aexp * acomm list

(* function and type parameterization,
 * which may extend another type. *)
type parameterization = typ Assoc.context

(* function parameters *)
(* arguments may have an optional parameterization *)
type params = (typ * string) list
type ret_typ = typ
(* function header -- our functions are not first-order! *)
type fn_typ = modification list * ret_typ * id * parameterization * params * metadata

(* General functions *)
type fn = fn_typ * acomm list

type frame =
    | FrameDim of string
    | FrameNum of int
type frame_decl = id * frame

type prototype_element =
    | ProtoObject of id * parameterization
    | ProtoFn of fn_typ
(* Name and list of declarations *)
type prototype = id * prototype_element list

type coordinate_element =
    | CoordObjectAssign of id * parameterization * typ
    | CoordFn of fn
(* Name, underlying prototype, dimension, and list of definitions *)
type coordinate = id * string * int * coordinate_element list

type global_var = modification list * storage_qual * typ * id * aexp option
type extern_element =
    | ExternFn of fn_typ
    | ExternVar of modification list * typ * id * metadata

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
    | Frame of frame_decl
    | Typ of id * parameterization * typ
    | Extern of extern_element
    | GlobalVar of global_var
    | Fn of fn

(* program *)
type prog = aterm list
