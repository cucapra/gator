open GatorAst

(* Type of a function invocation *)
type fn_inv = string * typ list 

(* Type definitions *)
(* Stores modification, supertype, 
 * and possible parameterization information for the type *)
(* type tau = modification list * parameterization * typ *)
type tau = bool * parameterization * typ

(* Variable definitions *)
(* Maps from variable names to the type of that variable *)
type gamma = typ

(* Dimension definitions *)
(* Stores dimension information for reference frames *)
type delta = dexp

(* Coordinate systems *)
(* Stores the following information about the given prototype or coordinate scheme:
 * If a coordinate scheme, the prototype implemented
 * The reference frame parameterization *)
type chi = parameterization * string option

(* Function definitions *)
(* Stores the full type each function overload *)
(* Also stores the name of the invoking class if applicable *)
type phi = (string option * fn_typ) list

(* Transformation context *)
(* Provides a map from a given type to all functions that  *)
(* type psi = string list *)
type psi = (typ * fn_inv) list

(* Special contexts for avoiding name duplication *)
(* We maintain the invariant for a given set of contexts: 
 * if a string 'x' is in lookup
 * then 'x' is in exactly one of the variant types of lookup 
 * otherwise 'x' is in none of the variant types *)
(* Note that the parameterization variable names are _not_ necessarily unique, 
 * so aren't tracked in the lookup *)
type exp_bindings = CGamma | CPhi
type typ_bindings = CTau | CChi | CDelta

(* Variant type for correctly abstracting storage of a new variable
 * Used with contexts to maintain the invariant described in 'lookup' *)
type binding = | Tau of tau | Gamma of gamma | Delta of delta 
    | Chi of chi | Phi of phi

(* The internal type of the lookup_contexts (which shouldn't be accessed directly) *)
type binding_contexts = {
  t : tau Assoc.context;
  g : gamma Assoc.context;
  d : delta Assoc.context;
  c : chi Assoc.context;
  p : phi Assoc.context;
  el : exp_bindings Assoc.context;
  tl : typ_bindings Assoc.context;
}

(* A type to contain every non-lookup context to simplify definitions *)
(* We use a preceding '_' to denote elements which shouldn't be accessed directly *)
(* Kept separate from binding_contexts since name repetition is permitted *)
type contexts = {
  ps : psi Assoc.context;
  pm : parameterization;
  (* parameterization of the wrapping scheme declaration *)
  scheme_pm : parameterization;
  externs : prog Assoc.context;
  meta : metadata;
  _bindings : binding_contexts
}