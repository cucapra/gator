(* IR for typed AST *)
open CoreAst

(* type with tags erased *)
type etyp =
  | UnitTyp
  | BoolTyp
  | IntTyp
  | FloatTyp
  | StringTyp
  | ArrTyp of etyp * constvar
  (* i.e. vec3[5] *)
  | ParTyp of string * etyp list
  | AnyTyp
  | GenTyp
  | ExactCodeTyp
  | StructureTyp
  | ClassTyp

(* expressions *)
type texp = exp * etyp

and exp =
  | Val of value
  | Var of id
  | Arr of texp list
  | Index of texp * texp
  | FnInv of id * etyp list * args
  (* last string of MethodInv and FieldSelect is used for the class name.
   * Useful for GLSL codegen. For struct, can set to dummy string. *)
  | MethodInv of exp option * string * etyp list * args * string
  | FieldSelect of exp option * id * string

and args = texp list

(* commands *)
type comm =
  | Skip
  | Print of texp
  | Exp of texp
  | Decl of etyp * id * texp
  | Assign of texp * texp
  | AssignOp of texp * string * texp
  | If of if_block * if_block list * comm list option
  (* if - elif list - else *)
  | For of comm * texp * comm * comm list
  | Return of texp option
  | ExactCodeComm of string

and if_block = texp * comm list

type parameterization = etyp Assoc.context
type params = (storage_qual list * etyp * string) list
type global_var = storage_qual list * etyp * string * texp option
type ret_typ = etyp
type fn_decl = ret_typ * id * parameterization * params
type fn = fn_decl * comm list
type structure_member = etyp * id
type structure = id * (structure_member list)

type class_member =
  | Field of visibility * etyp * id
  | Method of visibility * fn
type _class = id * (id option) * (class_member list)

type term =
  |  GlobalVar of global_var
  | Structure of structure
  | Class of _class
  | Fn of fn
type prog = term list
