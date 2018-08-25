open Lin_ops

type value = CoreAst.value

let dot (v1 : vec) (v2 : vec) : float =
  List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0. v1 v2

let normalize (v : vec) : vec = 
  let distance = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0. v) in
  List.map (fun x -> x /. distance) v

let rec vecf (length : int) (f : float) : vec =
  if length <= 0 then [] else f::(vecf (length - 1) f)
let rec vec_expand (vals : value list) : vec =
  match vals with
  | [] -> []
  | h::t -> (match h with
    | Num n -> (float_of_int n)::(vec_expand t)
    | Float f -> f::(vec_expand t)
    | VecLit v -> v@(vec_expand t)
    | _ -> failwith ("Bad argument to vecn " ^ (Util.string_of_value h))
  )
let rec vec_contract(length : int) (v : vec) : vec =
  match v with
  | [] -> []
  | h::t -> if (length > 0) then h::(vec_contract (length - 1) t) else []

let vecn (length : int) (args : value list) : vec =
  match args with
  | [] -> vecf length 0.
  | [Num n] -> vecf length (float_of_int n)
  | [Float f] -> vecf length f
  | [VecLit v] -> 
    if length < List.length v then vec_contract length v else vec_expand args
  | _ -> vec_expand args