let rec pretty (e : expr) : string =
    match e with
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Plus (e1, e2) -> (pretty e1) ^ " + " ^ (pretty e2)
    | _ -> failwith "Not implemented"
