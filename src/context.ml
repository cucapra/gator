module type ContextSig = sig
  
  (* A context must be a map from 'a to 'b *)
  type ('a, 'b) context

  (* Make a new empty context. *)
  val empty : ('a, 'b) context

  (* Left-biased merge of two contexts *)
  val merge : ('a, 'b) context -> ('a, 'b) context -> ('a, 'b) context

  (* Look up a variable by name and return the associated value. *)
  (* Raises Not_found if no binding *)
  val lookup : ('a, 'b) context -> 'a -> 'b

  (* Rebind var to value in context. *)
  val update : ('a, 'b) context -> 'a -> 'b -> ('a, 'b) context

  (* Produce bindings as an association list. *)
  val bindings : ('a, 'b) context -> ('a * 'b) list

  (* val state_to_string : ('a, 'b) context -> string *)
end

(* A representation for a general context *)
(* A context is simply a finite map from 'a to 'b  *)
(* This implementation uses association lists. *)
module Context : ContextSig = struct
  type ('a, 'b) context = ('a * 'b) list
  
  (* Make a new empty context. *)
  let empty = []

  (* Left-biased merge of two contexts *)
  let merge c1 c2 = c1 @ c2

  (* Look up a variable by name and return the associated letue. *)
  (* Raises Not_found if no binding *)
  let lookup c x = try List.assoc x c with | Not_found -> failwith "Undeclared variable"

  (* Rebind var to letue in context. *)
  let update c x v = if List.mem_assoc x c then failwith "Duplicate" else (x, v) :: c

  (* Produce bindings as an association list. *)
  let bindings c = c
end