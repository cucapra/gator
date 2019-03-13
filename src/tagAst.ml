(* AST definition of LinGL *)

open CoreAst

(* types *)
type typ =
    | AutoTyp
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TopVecTyp of int
    | BotVecTyp of int
    | VarTyp of id * typ list (* i.e. model or hom<model> *)
    | TransTyp of typ * typ
    | SamplerTyp of int (* i.e. sampler2D *)
    | SamplerCubeTyp
    | AbsTyp of id (* i.e. `t *)
    | ArrTyp of typ * constvar (* i.e. vec3[5] *)

type constrain =
    (* Special constraint types *)
    | AnyTyp
    | GenTyp
    | GenMatTyp
    | GenVecTyp
    | TypConstraint of typ

(* Global variables *)
type global_var = string * storage_qual * typ * value option

(* expressions *)
type exp =
    | Val of value
    | Var of string
    | Arr of exp list
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | As of exp * typ
    | In of exp * typ
    | FnInv of string * args * typ list (* function invocation *)

and args = exp list

type modification =
    | Coord
    | Canon

(* function parameterization,
 * which may extend another type. *)
type parameterization = constrain Assoc.context
type parameterization_decl = (string * modification option * constrain) list

(* function parameters *)
(* arguments may have an optional parameterization type *)
type params = (string * typ) list
type ret_type = typ
(* our functions are not first-order! *)
type fn_type = params * ret_type * parameterization
(* Note that the parameterization declaration is only useful when checking the function, not calling it *)
type fn_type_decl = params * ret_type * parameterization_decl
(* function declaration *)
type fn_decl = modification option * string * fn_type_decl
type extern_decl =
    | ExternFn of fn_decl
    | ExternVar of (typ * exp)

(* commands *)
type comm =
    | Skip
    | Print of exp
    | Inc of id
    | Dec of id
    | Decl of typ * typ option * string * exp
    | Assign of string * exp
    | AssignOp of string * binop * exp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * exp * comm * comm list
    | Return of exp option
    | FnCall of string * args * typ list
and if_block = exp * comm list

type fn = fn_decl * comm list

(* tag declaration statements *)
type tag_decl = modification option * string * parameterization_decl * typ

type global_var_or_fn =
    | GlobalVar of global_var
    | Fn of fn

(* program *)
(* Consists of list of (external) declare functions,
 * list of vector space tags,
 * and list of functions with at least one void main() function. *)
type prog = extern_decl list * tag_decl list * global_var_or_fn list
