open GatorAst

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
type delta = frame_dim

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

(* Special contexts for  *)
(* We maintain the invariant for a given set of contexts: 
 * if a string 'x' is in lookup
 * then 'x' is in exactly one of the variant types of lookup 
 * otherwise 'x' is in none of the variant types *)
(* Note that the parameterization variable names are _not_ necessarily unique, so aren't tracked in the lookup *)
type binding_context = | LTau | LGamma | LDelta | LChi | LOmega

(* Variant type for correctly abstracting storage of a new variable
 * Used with contexts to maintain the invariant described in 'lookup' *)
type binding = | Tau of tau | Gamma of gamma | Delta of delta 
    | Chi of chi | Omega of omega

(* The internal type of the lookup_contexts (which shouldn't be accessed directly) *)
type binding_contexts = {
  t : tau Assoc.context;
  g : gamma Assoc.context;
  d : delta Assoc.context;
  c : chi Assoc.context;
  o : omega Assoc.context;
  l : binding_context Assoc.context;
}

(* A type to contain every non-lookup context to simplify definitions *)
(* We use '_a' syntax to denote elements which shouldn't be accessed directly *)
type contexts = {
  m : mu Assoc.context;
  p : phi Assoc.context;
  ps : psi Assoc.context;
  pm : parameterization;
  meta : metadata;
  _bindings : binding_contexts
}