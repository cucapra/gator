open Ast

(* A context is a simply a finite map *)
module Context = struct
  
  (* A context must be a map from 'a to 'b *)
  type ('a, 'b) context

  (* Make a new empty context. *)
  val empty : ('a * 'b) context

  (* Left-biased merge of two contexts *)
  val merge : ('a * 'b) context -> ('a * 'b) context -> ('a * 'b) context

  (* Look up a variable by name and return the associated value. *)
  (* Raises Not_found if no binding *)
  val lookup : ('a * 'b) context -> id -> ('a * 'b) context

  (* Rebind var to value in context. *)
  val update : ('a * 'b) context -> id -> ('a * 'b) -> ('a * 'b) context

  (* Produce bindings as an association list. *)
  val bindings : ('a * 'b) context -> (id * 'a) list

  (* val state_to_string : ('a * 'b) context -> string *)
