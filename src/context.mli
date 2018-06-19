open Ast

(* A context is a finite map from strings to a thing. *)
module Context = struct
  
  (* A context must be a map from a string to 'a for some 'a *)
  type 'a context

  (* Make a new empty context. *)
  val empty : 'a context

  (* Left-biased merge of two contexts *)
  val merge : 'a context -> 'a context -> 'a context

  (* Look up a variable by name and return the associated value. *)
  (* Raises Not_found if no binding *)
  val lookup : 'a context -> id -> 'a context

  (* Rebind var to value in context. *)
  val update : 'a context -> id -> 'a -> 'a context

  (* Produce bindings as an association list. *)
  val bindings : 'a context -> (id * 'a) list

  (* val state_to_string : 'a context -> string *)
