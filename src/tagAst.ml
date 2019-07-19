(* AST definition of LinGL *)

open CoreAst

type dexp =
    | DimVar of string
    | DimNum of int
    | DimBinop of binop * dexp * dexp

(* types *)
type typ =
    | AutoTyp
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TopVecTyp of dexp
    | UntaggedVecTyp of int
    | BotVecTyp of int
    | VarTyp of id (* i.e. model *)
    | TransTyp of typ * typ
    | ParTyp of typ * typ list (* i.e. hom<model> or sampler2D<model, world> *)
    | SamplerTyp of int
    | SamplerCubeTyp
    | AbsTyp of id (* i.e. `t *)
    | ArrTyp of typ * constvar (* i.e. vec3[5] *)

type constrain =
    (* Special constraint types *)
    | AnyTyp
    | GenTyp
    | GenMatTyp
    | GenVecTyp
    | GenSpaceTyp
    | TypConstraint of typ

(* expressions *)
type exp =
    | Val of value
    | Var of string
    | Arr of exp list
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | As of exp * typ
    | In of exp * typ
    | FnInv of string * typ list * args (* function invocation *)

and args = exp list

type modification =
    | Canon
    | Space

(* function parameterization,
 * which may extend another type. *)
type parameterization = constrain Assoc.context
type parameterization_decl = (string * constrain) list

(* function parameters *)
(* arguments may have an optional parameterization type *)
type params = (modification list * typ * string) list
type ret_type = typ
(* our functions are not first-order! *)
type fn_type = params * ret_type * parameterization
(* Note that the parameterization declaration is only useful when checking the function, not calling it *)
type fn_type_decl = parameterization_decl * ret_type * params
(* function declaration *)
type fn_decl = modification list * string * fn_type_decl

(* commands *)
type comm =
    | Skip
    | Print of exp
    | Inc of id
    | Dec of id
    | Decl of modification list * typ * string * exp
    | Assign of string * exp
    | AssignOp of string * binop * exp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * exp * comm * comm list
    | Return of exp option
    | FnCall of typ * typ list * args (* e.g. f<model>(position) -- note that 'f' must be a string, but we treat it as a type to allow parsing of parametrized types *)
and if_block = exp * comm list

type tag_decl = string * parameterization_decl * typ
type fn = fn_decl * comm list
type global_var = modification list * storage_qual * typ * string * value option
type extern_decl =
    | ExternFn of fn_decl
    | ExternVar of (modification list * typ * exp)

(* Terms that make up a program *)
(* In any order, we have:
 * Tag Declarations of user types
 * External function declarations without bodies
 * Global variable declarations
 * Function declarations with bodies
 *)
type term =
    | TagDecl of tag_decl
    | ExternDecl of extern_decl
    | GlobalVar of global_var
    | Fn of fn

(* program *)
type prog = term list
