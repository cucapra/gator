open GatorAst

(* For readability, especially with psi *)
type fn_inv = string * typ list 

(* Type definitions *)
(* Stores supertype and possible parameterization information for the type *)
type tau = typ * parameterization

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
(* Stores the following information about the given coordinate scheme:
 * The prototype implemented
 * The require frame dimension
 * A map to the underlying type of each every member element / a command list for each member function *)
type chi = string * dexp * coordinate_element Assoc.context

(* Prototypes *)
(* Stores parameterization for objects and function permissions provided by prototypes *)
type omega = prototype_element Assoc.context

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
type binding_context = | CTau | CGamma | CDelta | CChi | COmega | CPhi

(* Variant type for correctly abstracting storage of a new variable
 * Used with contexts to maintain the invariant described in 'lookup' *)
type binding = | Tau of tau | Gamma of gamma | Delta of delta 
    | Chi of chi | Omega of omega | Phi of phi

(* The internal type of the lookup_contexts (which shouldn't be accessed directly) *)
type binding_contexts = {
  t : tau Assoc.context;
  g : gamma Assoc.context;
  d : delta Assoc.context;
  c : chi Assoc.context;
  o : omega Assoc.context;
  p : phi Assoc.context;
  l : binding_context Assoc.context;
}

(* A type to contain every non-lookup context to simplify definitions *)
(* We use '_a' syntax to denote elements which shouldn't be accessed directly *)
type contexts = {
  m : mu Assoc.context;
  ps : psi Assoc.context;
  pm : parameterization;
  meta : metadata;
  _bindings : binding_contexts
}