open GatorAst
open CheckUtil

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

val init : metadata -> contexts

val w_m : contexts -> mu Assoc.context -> contexts
val w_p : contexts -> phi Assoc.context -> contexts
val w_ps : contexts -> psi Assoc.context -> contexts
val w_pm : contexts -> parameterization -> contexts
val w_meta : contexts -> metadata -> contexts

val find : contexts -> string -> binding option
val bind : contexts -> string -> binding -> contexts
val clear : contexts -> binding_context -> contexts