(* A association list representation for contexts *)
(* A context is simply a finite map from strings to 'a  *)
type 'a context = (string * 'a) list

(* Make a new empty context. *)
let empty = []

(* Left-biased merge of two contexts *)
let union c1 c2 = c1 @ c2

(* Look up a variable by name and return the associated letue. *)
(* Raises Not_found if no binding *)
let lookup x c = try List.assoc x c with _ -> failwith ("Undefined association member: " ^ x)

(* Remove var from context *)
let remove x c = List.remove_assoc x c

(* Rebind var to value in context. *)
let update x v c = (x, v) :: (List.remove_assoc x c)

(* Produce bindings as an association list. *)
let bindings c = c
let keys c = List.map fst c
let values c = List.map snd c

(* Generates a context from a list *)
let create l = List.fold_left (fun acc (s, v) -> update s v acc) empty (List.rev l)

(* Check var exists in context *)
let mem x c = List.mem_assoc x c

let map f c = List.map (fun (x, v) -> (x, f v)) c

let size c = List.length c

let to_string_sep f sep c = String.concat sep (List.map (fun (l, r) -> l ^ " : " ^ f r) c)
let to_string f c = to_string_sep f ", " c
