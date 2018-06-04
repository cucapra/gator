open Ast
open State

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

let rec eval_aexp (e : aexp) (s : state) : avalue =
    match e with
    | Var (s, _) -> failwith "Unimplemented"
    | Vec (a, _) -> failwith "Unimplemented"
    | Mat (a, _) -> failwith "Unimplemented"
    | Dot (a1, a2) -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Plus (a1, a2) -> failwith "Unimplemented"
    | Times (a1, a2) -> failwith "Unimplemented"
    | Minus (a1, a2) -> failwith "Unimplemented"
    | LCompTimes (a1, a2) -> failwith "Unimplemented"
    | LTrans (a1, a2) -> failwith "Unimplemented"

let rec eval_bexp (e : bexp) (s : state) : bvalue =
    match e with
    | True -> true 
    | False -> false
    | Eq (a1, a2) -> failwith "Unimplemented"
    | Leq (a1, a2) -> failwith "Unimplemented"
    | Or (b1, b2) -> eval_bexp(b1, s) || eval_bexp(b2, s)
    | And (b1, b2) -> eval_bexp(b1, s) && eval_bexp(b2, s)
    | Not b -> !eval_bexp(b, s)

let rec eval_comm (c : comm) (s : state) : state =
    match c with
    | Skip
    | Print a -> print_int (eval_aexp a s); s
    | Decl (_, s, e) -> 
    | Seq (c1, c2) -> eval_comm c2 (eval_comm c1 s)
    | If (b, c1, c2) -> if (eval_bexp b s) then (eval_comm c1 s) else (eval_comm c2 s)
