(* A representation for states (a.k.a. stores, environments, heaps, etc.). *)

open Ast

(* A state is a finite map from variables to values. *)
(* This implementation uses association lists. *)
type state = (id * value) list

let merge = (@)

(* Produce bindings as an association list. *)
let bindings (s : state) : (id * value) list = s

let to_string (v : value) : string =
  match v with
    | _ -> failwith "Unimplemented"

let state_to_string (s:state) : string =
  List.fold_left
    (fun acc (x,v) -> Printf.sprintf "  %s -> %s\n%s" x (to_string v) acc)
    ""
    (bindings s)

(* Make a new empty state. *)
let make () = []

(* Look up a variable by name and return the associated value. *)
(* Raises Not_found if no binding *)
let lookup (s : state) (var : id) : value =
  try List.assoc var s
  with Not_found -> failwith ("Uninitialized variable " ^ var)

(* Rebind var to value in state. *)
let update (s : state) (var : id) (value : value) : state =
  (var, value) :: s

(* Recursive update: assuming v is a closure of the form (e, s), *)
(* return s', an extension of s that rebinds f to (e, s'). *)
(* In this way, f can refer to itself. *)
(* This is useful for implementing `let rec`. *)
(*let rec_update (v : avalue) (f : id) : state =
  match v with
    | Closure (g, s) ->
       let rec u = (f, Closure (g, u)) :: s in u
    | _ ->
       failwith "Improper value for let rec"*)
