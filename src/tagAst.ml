(* AST definition of LinGL *)

open CoreAst

(* tag types *)
type tag_typ =
    | TopTyp of int
    | BotTyp of int
    | VarTyp of id

(* types *)
type typ =
    | AutoTyp
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tag_typ
    | TransTyp of tag_typ * tag_typ
    | SamplerTyp of int (* i.e. sampler2D *)
    | AbsTyp of string 
    | AppTyp of string * typ (* type application *)
    | GenTyp

(* expressions *)
type exp =
    | Val of value
    | Var of string
    | Arr of exp list
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | FnInv of string * args * typ list (* function invocation *)

and args = exp list

(* function parameterization,
 * which may extend another type. *)
type parametrization = (typ * typ option) list

(* function parameters *)
type params = (string * typ) list
type ret_type = typ
(* our functions are not first-order! *)
type fn_type = params * ret_type * parametrization
(* function declaration *)
type fn_decl = string * fn_type

(* commands *)
type comm =
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | Assign of string * exp
    | If of exp * comm list * comm list
    | Return of exp option
    | FnCall of string * args * typ list

type fn = fn_decl * comm list

(* tag declaration statements *)
type tag_decl = string * typ

(* program *)
(* Consists of list of (external) declare functions,
 * list of vector space tags,
 * and list of functions with at least one void main() function. *)
type prog =
    | Prog of fn_decl list * tag_decl list * fn list

