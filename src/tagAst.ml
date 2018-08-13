(* AST definition of LinGL *)

open CoreAst

(* tag types *)
type tag_typ =
    | TopTyp of int
    | BotTyp of int
    | VarTyp of id

(* "Normal" type, as apposed to polymorphic types.
 * While the need for separation for these would be useful in
 * the context of let polymorphism, it is unclear if we 
 * need this. *)
type n_typ =
    UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tag_typ
    | TransTyp of tag_typ * tag_typ
    | SamplerTyp of int (* i.e. sampler2D *)

(* Polymorphic type.
 * Not to be confused with a certain kind of semiconductors. *)
type p_typ = string

(* types *)
type typ = 
    NTyp of n_typ 
    | PTyp of p_typ
    
(* expressions *)
type exp =
    | Val of value
    | Var of string
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | VecTrans of int * tag_typ (* vec3(<vec4>), vec4(<vec3>) *)
    | FnInv of string * args * parametrization list option (* function invocation *)

and args = exp list

(* function parameterization,
 * which may extend another type. *)
and parametrization = typ * typ option

(* function parameters *)
type params = (string * typ) list
type ret_type = typ
(* our functions are not first-order! *)
type fn_type = params * ret_type * parametrization list option
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
    | FnCall of string * args 

type fn = fn_decl * comm list

(* tag declaration statements *)
type tag_decl = string * typ

(* program *)
(* Consists of list of (external) declare functions,
 * list of vector space tags,
 * and list of functions with at least one void main() function. *)
type prog =
    | Prog of fn_decl list * tag_decl list * fn list

