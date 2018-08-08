(* A representation for a general context *)
(* A context is simply a finite map from 'a to 'b  *)
(* This implementation uses association lists. *)
type 'b context = (string * 'b) list

(* Make a new empty context. *)
let empty = []

(* Left-biased merge of two contexts *)
let merge c1 c2 = c1 @ c2

(* Look up a variable by name and return the associated letue. *)
(* Raises Not_found if no binding *)
let lookup (x: string) c = try List.assoc x c with _ -> failwith ("Undefined tag or variable: " ^ x)

(* Rebind var to value in context. *)
let update x v c = let c' = (if List.mem_assoc x c then List.remove_assoc x c else c) in (x, v) :: c'

(* Produce bindings as an association list. *)
let bindings c = c

(* Check var exists in context *)
let mem x c = List.mem_assoc x c

