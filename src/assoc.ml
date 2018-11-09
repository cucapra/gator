(* A representation for a general context *)
(* A context is simply a finite map from strings to 'a  *)
(* This implementation uses association lists. *)
type 'a context = (string * 'a) list

(* Make a new empty context. *)
let empty = []

(* Left-biased merge of two contexts *)
let merge c1 c2 = c1 @ c2

(* Look up a variable by name and return the associated letue. *)
(* Raises Not_found if no binding *)
let lookup (x: string) c = try List.assoc x c with _ -> failwith ("Undefined association member: " ^ x)

(* Rebind var to value in context. *)
let update x v c = let c' = (if List.mem_assoc x c then List.remove_assoc x c else c) in (x, v) :: c'

(* Produce bindings as an association list. *)
let bindings c = c

(* Generates a context from a list *)
let gen_context l = List.fold_left (fun acc (s, v) -> update s v acc) empty (List.rev l)

(* Check var exists in context *)
let mem x c = List.mem_assoc x c

let map f c = List.map (fun (s, x) -> (s, f x)) c

let size c = List.length c

let to_string f c = let pair_f (x : (string * 'a)) : string = (fst x) ^ " : " ^ (f (snd x)) in
  "[" ^ (String.concat "," (List.map pair_f c)) ^ "]"
