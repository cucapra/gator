open Ast

let int_op (v : value * state) : int =
    match (fst v) with
    | Int x -> x
    | Bool x -> failwith "Cannot do an integer operation on a boolean"
    | Null -> failwith "Cannot operate on skip"

let bool_op (v : value * state) : bool =
    match (fst v) with
    | Int x -> failwith "Cannot do a boolean operation on an integer"
    | Bool x -> x
    | Null -> failwith "Cannot operate on skip"

let rec l_to_s = function
    | [] -> ""
    | h::t -> "(" ^ (fst h) ^ "," ^ (string_of_int (snd h)) ^ "), " ^ (l_to_s t)

let rec eval (e : expr) (s : state) : value * state =
    match e with
    | Int i -> (Int i, s)
    | Bool b -> (Bool b, s)
    | Id x -> (try (Int (List.assoc x s), s) with
      Not_found -> failwith (l_to_s s))(*"Variable " ^ x ^ " has no value assigned to it")*)
    (* AEXP *)
    | Plus (e1, e2) -> (Int ((int_op (eval e1 s)) + (int_op (eval e2 s))), s)
    | Minus (e1, e2) -> (Int ((int_op (eval e1 s)) - (int_op (eval e2 s))), s)
    | Times (e1, e2) -> (Int ((int_op (eval e1 s)) * (int_op (eval e2 s))), s)
    | Div (e1, e2) -> (Int ((int_op (eval e1 s)) / (int_op (eval e2 s))), s)
    (* BEXP *)
    | Eq (e1, e2) -> (Bool ((int_op (eval e1 s)) = (int_op (eval e2 s))), s)
    | Leq (e1, e2) -> (Bool ((int_op (eval e1 s)) <= (int_op (eval e2 s))), s)
    | Or (e1, e2) -> (Bool ((bool_op (eval e1 s)) || (bool_op (eval e2 s))), s)
    | And (e1, e2) -> (Bool ((bool_op (eval e1 s)) && (bool_op (eval e2 s))), s)
    | Not e -> (Bool (not (bool_op (eval e s))), s)
    (* COM *)
    | Skip -> (Null, s)
    | Assign (x, e) -> (Null, (x, (int_op (eval e s))) :: s)
    | Sequence (e1, e2) -> (eval e2 (snd (eval e1 s)))
    | Cond (eb, e1, e2) -> if (bool_op (eval eb s)) then (eval e1 s) else (eval e2 s)
    | While (eb, c) -> eval (Cond (eb, Sequence (c, While (eb, c)), Skip)) s
