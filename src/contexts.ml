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

(* Produces an empty set of gator contexts with a starting metadata *)
let init meta = let b = 
  {t=Assoc.empty; g=Assoc.empty; d=Assoc.empty; 
  c=Assoc.empty; o=Assoc.empty; l=Assoc.empty} in
  {m=Assoc.empty; p=Assoc.empty; ps=Assoc.empty; pm=Assoc.empty; meta=meta; _bindings=b }

let w_m cx m' = {cx with m=m'}
let w_p cx p' = {cx with p=p'}
let w_ps cx ps' = {cx with ps=ps'}
let w_pm cx pm' = {cx with pm=pm'}
let w_meta cx meta' = {cx with meta=meta'}

(* Finds which context in which to find the given string *)
let find cx x =
  if Assoc.mem x cx._bindings.l then match Assoc.lookup x cx._bindings.l with
  | LTau -> Some (Tau (Assoc.lookup x cx._bindings.t))
  | LGamma -> Some (Gamma (Assoc.lookup x cx._bindings.g))
  | LDelta -> Some (Delta (Assoc.lookup x cx._bindings.d))
  | LChi -> Some (Chi (Assoc.lookup x cx._bindings.c))
  | LOmega -> Some (Omega (Assoc.lookup x cx._bindings.o))
  else None

(* Binds a string with value to the correct lookup context *)
let bind (cx : contexts) (x : string) (b : binding) : contexts =
  if Assoc.mem x cx._bindings.l
  then raise (TypeExceptionMeta ("Duplicate use of the name " ^ x, cx.meta)) else 
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  match b with
  | Tau t' ->   update_bindings { _b with l=Assoc.update x LTau _b.l; t=Assoc.update x t' _b.t }
  | Gamma g' -> update_bindings { _b with l=Assoc.update x LTau _b.l; g=Assoc.update x g' _b.g }
  | Delta d' -> update_bindings { _b with l=Assoc.update x LTau _b.l; d=Assoc.update x d' _b.d }
  | Chi c' ->   update_bindings { _b with l=Assoc.update x LTau _b.l; c=Assoc.update x c' _b.c }
  | Omega o' -> update_bindings { _b with l=Assoc.update x LTau _b.l; o=Assoc.update x o' _b.o }

(* Clears the given lookup context of elements *)
let clear (cx : contexts) (b : binding_context) =
  let update_bindings b' = {cx with _bindings=b'} in
  let _b = cx._bindings in
  let build_l l xs = Assoc.gen_context (List.fold_left (fun acc (x, v) -> 
    if List.mem x xs then acc else (x, v)::acc) [] l) in
  let clear c = build_l (Assoc.bindings _b.l) (List.map fst (Assoc.bindings c)) in
  match b with
  | LTau ->   update_bindings { _b with l=clear _b.t; t=Assoc.empty }
  | LGamma -> update_bindings { _b with l=clear _b.g; g=Assoc.empty }
  | LDelta -> update_bindings { _b with l=clear _b.d; g=Assoc.empty }
  | LChi ->   update_bindings { _b with l=clear _b.c; c=Assoc.empty }
  | LOmega -> update_bindings { _b with l=clear _b.o; o=Assoc.empty }