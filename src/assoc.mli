(* A context must be a map from a string to 'a *)
type 'a context

(* Make a new empty context. *)
val empty : 'a context

(* Left-biased merge of two contexts *)
val union : 'a context -> 'a context -> 'a context

(* Checks if variable is in context *)
val mem : string -> 'a context -> bool

(* Look up a variable by name and return the associated value. *)
(* Raises Not_found if no binding *)
val lookup : string -> 'a context -> 'a

(* Remove var from context *)
val remove : string -> 'a context -> 'a context

(* Rebind var to value in context *)
val update : string -> 'a -> 'a context -> 'a context

(* Produce bindings as an association list. *)
val bindings : 'a context -> (string * 'a) list

(* Produces the list of keys of an association list *)
val keys : 'a context -> string list

(* Produces the list of values of an association list *)
val values : 'a context -> 'a list

(* Generates a context from a list *)
val create : (string * 'a) list -> 'a context

(* val state_to_string : ('a, 'a) context -> string *)

val map : ('a -> 'b) -> 'a context -> 'b context

val size : 'a context -> int

(* Gives a string resprentation of this association list, seperated by sep *)
val to_string_sep : ('a -> string) -> string -> 'a context -> string

(* String representation seperated by ", " *)
val to_string : ('a -> string) -> 'a context -> string