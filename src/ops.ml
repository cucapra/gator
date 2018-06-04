open Ast
open State

let rec l_to_s = function
    | [] -> ""
    | h::t -> "(" ^ (fst h) ^ "," ^ (string_of_int (snd h)) ^ "), " ^ (l_to_s t)

let rec eval_aexp (e : aexp) (s : state) : avalue =
    match e with
    | Const c -> failwith "Unimplemented"
    | Var s -> failwith "Unimplemented"
    | Vec (a, _) -> failwith "Unimplemented"
    | Mat (a, _) -> failwith "Unimplemented"
    | Dot (a1, a2) -> failwith "Unimplemented"
    | Norm a -> failwith "Unimplemented"
    | Plus (a1, a2) -> failwith "Unimplemented"
    | Times (a1, a2) -> failwith "Unimplemented"
    | Minus (a1, a2) -> failwith "Unimplemented"
    | LCompTimes (a1, a2) -> failwith "Unimplemented"
    | LTrans (a1, a2) -> failwith "Unimplemented"

let rec eval_bexp (e : bexp) (s : state) : bool =
    match e with
    | True -> true 
    | False -> false
    | Eq (a1, a2) -> failwith "Unimplemented"
    | Leq (a1, a2) -> failwith "Unimplemented"
    | Or (b1, b2) -> (eval_bexp b1 s) || (eval_bexp b2 s)
    | And (b1, b2) -> (eval_bexp b1 s) && (eval_bexp b2 s)
    | Not b -> not (eval_bexp b s)

let eval_print (e : exp) (s : state) : unit =
    match e with
    | Aexp a -> (match (eval_aexp a s) with 
        | Int i -> print_int i
        | Float f -> print_float f
        | VecLit v -> failwith "Unimplemented"
        | MatLit m -> failwith "Unimplemented")
    | Bexp b -> (print_string (string_of_bool (eval_bexp b s)))   

let rec eval_comm (c : comm) (s : state) : state =
    match c with
    | Skip -> s
    | Print e -> eval_print e s; s
    | Decl (_, s, e) -> failwith "unimplemented"
    | Seq (c1, c2) -> eval_comm c2 (eval_comm c1 s)
    | If (b, c1, c2) -> if (eval_bexp b s) then (eval_comm c1 s) else (eval_comm c2 s)