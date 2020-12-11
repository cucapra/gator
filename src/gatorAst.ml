(* AST definition of LinGL *)

open CoreAst

type metadata = Lexing.position
type 'a astNode = 'a * metadata
type dexp = DimVar of string | DimNum of int | DimPlus of dexp * dexp

(* type dtyp = 
    | BaseTyp of string
    | MultTyp of dtyp * dtyp *)
(* types *)
type typ =
  | BotTyp
  (* Useful exactly for empty lists *)
  | AutoTyp
  | UnitTyp
  | BoolTyp
  | IntTyp
  | FloatTyp
  | StringTyp
  (* | DimTyp of dtyp *)
  | ThisTyp
  (* Used for coordinate/class declarations *)
  | Literal of typ
  | ArrTyp of typ * dexp
  (* i.e. float[5] or bool[2][3] *)
  | MemberTyp of typ * typ
  (* i.e. cart.point *)
  | ParTyp of string * typ list
  (* i.e. color or matrix<model, world> *)
  (* Note that ParTyp is overloaded to act as the variable type when parameterization length is 0 *)
  | AnyTyp
  | AnyFrameTyp
  (* top type for frames (used in prototype declarations) *)
  | FrameTyp of dexp
  (* i.e. frame<3> or frame<n> *)
  | GenTyp
  | GenArrTyp of typ
  | ExactCodeTyp
  | StructureTyp (* The supertype of structs *)


(* expressions *)
type aexp = exp astNode

and exp =
  | Val of value
  | Var of string
  | Arr of aexp list
  | Index of aexp * aexp
  (* a[3] -- special operation that can't be user-defined *)
  | As of aexp * typ
  | In of aexp * typ
  | FnInv of string * typ list * args
  | FieldSelect of exp * id

(* function invocation *)
and args = aexp list

type modification =
  | With of typ * string list * bool
  (*bool to  specify whether we are restricting ops on types*)
  | Canon
  | External
  | Storage_Qualifier of storage_qual

(* commands *)
type acomm = comm astNode

and comm =
  | Skip
  | Print of aexp
  | Exp of aexp
  | Decl of modification list * typ * string * aexp
  | Assign of aexp * aexp
  | AssignOp of aexp * string * aexp
  | If of if_block * if_block list * acomm list option
  (* if - elif list - else *)
  | For of acomm * aexp * acomm * acomm list
  | Return of aexp option
  | ExactCodeComm of string

and if_block = aexp * acomm list

(* function and type parameterization,
 * which may extend another type. *)
type parameterization = (typ * bool) Assoc.context

(* function parameters *)
(* arguments may have an optional parameterization *)
type params = (modification list * typ * string) list
type ret_typ = typ

(* function header -- our functions are not first-order! *)
type fn_typ = modification list * ret_typ * id * params * metadata

(* General functions *)
type fn = fn_typ * acomm list
type frame = id * dexp

type prototype_element =
  | ProtoObject of modification list * id * typ option
  | ProtoFn of fn_typ

(* Name and list of declarations *)
type aprototype_element = prototype_element astNode
type prototype = id * aprototype_element list

type coordinate_element =
  | CoordObjectAssign of modification list * id * typ
  | CoordFn of fn

(* Name, underlying prototype, dimension, and list of definitions *)
type acoordinate_element = coordinate_element astNode
type coordinate = modification list * id * string * acoordinate_element list
type global_var = modification list * typ * id * aexp option

type typedef = typ * id

type structure_member = typ * id
type structure = id * (structure_member list) * metadata

type visibility = 
  | Private
  | Public
  | Protected
  | Default

type class_member =
  | Field of visibility * typ * id
  | Method of visibility * fn
type _class = id * (id option) * (class_member list)

(* Terms that make up a program *)
(* In any order, we have:
 * ExactCode for exact code insertion
 * Frame Declarations of user types
 * External function declarations without bodies
 * Global variable declarations
 * Function declarations with bodies
 * Typedef to create a new alias for a type
 *)
type aterm = term astNode

and term =
  | Using of string
  | ExactCode of string
  | Prototype of prototype
  | Coordinate of coordinate
  | Frame of frame
  | Typ of modification list * id * typ
  | GlobalVar of global_var
  | Fn of fn
  | Structure of structure
  | Typedef of typedef (* Should be removed from ast before typechecking *)
  | Class of _class

(* program *)
type prog = aterm list
