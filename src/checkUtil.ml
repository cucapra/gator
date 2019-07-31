open Util
open GatorAst
open GatorAstPrinter

exception TypeException of string
exception TypeExceptionMeta of string * metadata
exception DimensionException of int * int

(* For readability, especially with psi *)
type fn_inv = string * typ list 

(* Type definitions *)
(* Stores supertype and possible parameterization information for the tag *)
type tau = parameterization * typ

(* Type and function modifiers *)
(* Specifically the canonical and with keywords *)
type mu = modification list

(* Variable definitions *)
(* Maps from variable names to the type of that variable *)
type gamma = typ

(* Dimension definitions *)
(* Stores dimension information for reference frames *)
type delta = dexp

(* Coordinate systems *)
(* Stores type of each every member element and command list for each function in coordinate systems *)
type chi = coordinate_element

(* Prototypes *)
(* Stores parameterization for objects and function permissions provided by prototypes *)
type omega = prototype_element

(* Function definitions *)
(* Stores the full type and parameterization of each function *)
type phi = fn_typ

(* Transformation context *)
(* Effectively has the type 'start->(target, f<pml>) list' for types start and target (both restricted implicitely to var types), *)
(* function/matrix name f, and function parameter list pml *)
(* Note that the resulting thing could be a call with a concrete parameterization, hence the typ list (which is empty for matrices) *)
type psi = (typ * fn_inv) list

let typ_ignore (t : typ) : unit = ignore t
let constrain_ignore (c : constrain) : unit = ignore c
let string_of_tau (t : tau) =
    fun (pm, t') -> "(" ^ string_of_parameterization pm ^ ", " ^ string_of_typ t' ^ ")"
let string_of_fn_inv ((s, tl) : fn_inv) = s ^ "<" ^ string_of_list string_of_typ tl ^ ">"
let string_of_psi (ps : psi) =
    fun x -> string_of_list (fun (t, p) -> "(" ^ string_of_typ t ^ ", " ^ string_of_fn_inv p ^ ")") x
let line_number (meta : metadata) = ("Line: " ^ string_of_int(meta.pos_lnum))