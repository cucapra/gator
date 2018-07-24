type vec = CoreAst.vec
type mat = CoreAst.mat

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

let sm_mult (s : float) (m : mat) : mat = 
  List.map (fun x -> List.map (fun y -> s *. y) x) m

let vc_mult (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x *. y) v1 v2

let mc_mult (m1 : mat) (m2 : mat) : mat =
  List.map2 (fun x y -> vc_mult x y) m1 m2

let rec transpose (m : mat) : mat =
  (*https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists*)
  match m with
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let vec_mult (v : vec) (m : mat) : vec =
  List.rev (List.fold_left 
    (fun vacc mv -> (List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0. v mv)::vacc)
    [] (transpose m))

let mat_mult (m1 : mat) (m2 : mat) : mat =
  List.rev (List.fold_left (fun acc v -> (vec_mult v m2)::acc) [] m1)

let dot (v1 : vec) (v2 : vec) : float =
  List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0. v1 v2

let norm (v : vec) : float = 
  sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0. v)

let vec_eq (v1 : vec) (v2 : vec) : bool =
  List.fold_left2 (fun acc x y -> acc && (x = y)) true v1 v2

let mat_eq (m1 : mat) (m2 : mat) : bool =
  List.fold_left2 (fun acc x y -> acc && (vec_eq x y)) true m1 m2