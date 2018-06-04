
open Ast

(* A state is a finite map from variables to values. *)
type state

(* Values are ints, bools, and closures. *)
type value = Number of int | Boolean of bool | Closure of exp * state | Error

(* Make a new empty state. *)
val make : unit -> state

(* Left-biased merge of two states *)
val merge : state -> state -> state

(* Look up a variable by name and return the associated value. *)
(* Raises Not_found if no binding *)
val lookup : state -> id -> value

(* Rebind var to value in state. *)
val update : state -> id -> value -> state

(* Recursive update: assuming v is a closure of the form (e, s), *)
(* return s', an extension of s that rebinds f to (e, s'). *)
(* In this way, f can refer to itself in e. *)
(* This is useful for implementing `let rec`. *)
val rec_update : value -> id -> state

(* Produce bindings as an association list. *)
val bindings : state -> (id * value) list

val to_string : value -> string

val state_to_string : state -> string
