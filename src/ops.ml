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

let rec eval_aexp (e : aexp) (s : state) (d : tags) : value * state =
    | Var (s, _) -> failwith "Unimplemented"
    | Vec (a, _) -> failwith "Unimplemented"
    | Mat (a, _) -> failwith "Unimplemented"
    | Dot (a, a) -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Plus (a, a) -> failwith "Unimplemented"
    | Times (a, a) -> failwith "Unimplemented"
    | Minus (a, a) -> failwith "Unimplemented"
    | LCompTimes (a, a) -> failwith "Unimplemented"
    | LTrans (a, a) -> failwith "Unimplemented"
