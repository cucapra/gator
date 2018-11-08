(* A context must be a map from a string to 'a *)
type 'a context

(* Make a new empty context. *)
val empty : 'a context

(* Left-biased merge of two contexts *)
val merge : 'a context -> 'a context -> 'a context

(* Look up a variable by name and return the associated value. *)
(* Raises Not_found if no binding *)
val lookup : string -> 'a context -> 'a

(* Rebind var to value in context. *)
val update : string -> 'a -> 'a context -> 'a context

(* Produce bindings as an association list. *)
val bindings : 'a context -> (string * 'a) list

(* Checks if variable is in context *)
val mem : string -> 'a context -> bool
(* val state_to_string : ('a, 'a) context -> string *)

val map : ('a -> 'b) -> 'a context -> 'b context

val size : 'a context -> int

val to_string : ('a -> string) -> 'a context -> string