type vec = float list
type mat = float list list

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