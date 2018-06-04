type vec = Ast.vec
type mat = Ast.mat

let vec_to_mat (v : vec) : mat = [v]
let mat_to_vec (m : mat) : vec = 
  if (List.length m = 1) then (List.flatten m) else (failwith "Invalid Argument");;

let vec_add (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x +. y) v1 v2

let mat_add (m1 : mat) (m2 : mat) : mat = 
  List.map2 (fun x y -> vec_add x y) m1 m2

let sv_mult (s : float) (v : vec) : vec = 
  List.map (fun x -> s *. x) v

let sm_mult (s : float) (m : mat) : mat = 
  List.map (fun x -> List.map (fun y -> s *. y) x) m

let vc_mult (v1 : vec) (v2 : vec) : vec =
  List.map2 (fun x y -> x *. y) v1 v2

let mc_mult (m1 : mat) (m2 : mat) : mat =
  List.map2 (fun x y -> vc_mult x y) m1 m2

let vec_mult (v : vec) (m : mat) : vec =
  failwith "Unimplemented"

let mat_mult (m1 : mat) (m2 : mat) : mat =
  failwith "Unimplemented"