(* A context must be a map from 'a to 'b *)
type 'b context

(* Make a new empty context. *)
val empty : 'b context

(* Left-biased merge of two contexts *)
val merge : 'b context -> 'b context -> 'b context

(* Look up a variable by name and return the associated value. *)
(* Raises Not_found if no binding *)
val lookup : string -> 'b context -> 'b

(* Rebind var to value in context. *)
val update : string -> 'b -> 'b context -> 'b context

(* Produce bindings as an association list. *)
val bindings : 'b context -> (string * 'b) list

(* Checks if variable is in context *)
val mem : string -> 'b context -> bool
(* val state_to_string : ('a, 'b) context -> string *)