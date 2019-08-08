type vec = CoreAst.vec
type mat = CoreAst.mat

let option_map (f: ('a -> 'b)) (o: 'a option) : 'b option =
  match o with
  | None -> None
  | Some x -> Some (f x)

let as_vec_safe (a : CoreAst.value list) : vec option =
  List.fold_right (fun x acc -> match x with 
    | CoreAst.Float f -> option_map (fun y -> f::y) acc
    | _ -> None) a (Some [])

let as_vec (a : CoreAst.value list) : vec =
  match as_vec_safe a with
  | Some v -> v
  | _ -> failwith "Expected float"

let as_mat_safe (a : CoreAst.value list) : mat option =
  List.fold_right (fun x acc -> match x with 
    | CoreAst.ArrLit a' -> option_map (fun y -> as_vec a'::y) acc
    | _ -> None) a (Some [])

let as_mat (a : CoreAst.value list) : mat =
  match as_mat_safe a with
  | Some m -> m
  | _ -> failwith "Expected vector"

let arr_of_vec (v : vec) : CoreAst.value =
  CoreAst.ArrLit (List.fold_right (fun x acc -> CoreAst.Float x::acc) v [])
let arr_of_mat (m : mat) : CoreAst.value =
  CoreAst.ArrLit (List.fold_right (fun x acc -> arr_of_vec x::acc) m [])
let vec_to_mat (v : vec) : mat = [v]
let mat_to_vec (m : mat) : vec = 
  if (List.length m = 1) then (List.flatten m) else (failwith "Invalid Argument")

let vec_add (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x +. y) v1 v2

let mat_add (m1 : mat) (m2 : mat) : mat = 
  List.map2 (fun x y -> vec_add x y) m1 m2

let vec_sub (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x -. y) v1 v2

let mat_sub (m1 : mat) (m2 : mat) : mat = 
  List.map2 (fun x y -> vec_sub x y) m1 m2

let sv_mult (s : float) (v : vec) : vec = 
  List.map (fun x -> s *. x) v

let iv_mult (s : int) (v : vec) : vec = 
  List.map (fun x -> (float_of_int (s * (int_of_float x)))) v

let sm_mult (s : float) (m : mat) : mat = 
  List.map (fun x -> List.map (fun y -> s *. y) x) m

let im_mult (s : int) (m : mat) : mat = 
  List.map (fun x -> List.map (fun y -> float_of_int (s * (int_of_float y))) x) m

let vc_mult (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x *. y) v1 v2

let mc_mult (m1 : mat) (m2 : mat) : mat =
  List.map2 (fun x y -> vc_mult x y) m1 m2

let rec transpose (m : 'a list) : 'a list =
  (*https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists*)
  match m with
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let vec_mult (m : mat) (v : vec) : vec =
  List.rev (List.fold_left
    (fun vacc mv -> (List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0. v mv)::vacc)
    [] m)

let mat_mult (m1 : mat) (m2 : mat) : mat =
  List.rev (List.fold_left (fun acc v -> (vec_mult (transpose m2) v)::acc) [] m1)

let vec_eq (v1 : vec) (v2 : vec) : bool =
  List.fold_left2 (fun acc x y -> acc && (x = y)) true v1 v2

let mat_eq (m1 : mat) (m2 : mat) : bool =
  List.fold_left2 (fun acc x y -> acc && (vec_eq x y)) true m1 m2