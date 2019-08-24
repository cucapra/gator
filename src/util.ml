open CoreAst
(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string

let flip f x y = f y x
let compose f g = fun x -> f (g x)
let (|-) = compose
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

(* Cause for some reason Option.map doesn't exist? *)
let option_map (f: 'a -> 'b) (o: 'a option) : 'b option =
  match o with
  | None -> None
  | Some x -> Some (f x) 

let tr_fst ((x, _, _): 'a * 'b * 'c) : 'a = x
let tr_snd ((_, x, _): 'a * 'b * 'c) : 'b = x
let tr_thd ((_, _, x): 'a * 'b * 'c) : 'c = x

let nonempty x = List.length x > 0
let string_of_option_removed (f : 'a -> string) (o : 'a option) : string =
  match o with
  | Some v -> f v
  | None -> ""

let string_of_pair (a: string) (b: string) : string = 
  "(" ^ a ^ ", " ^ b ^ ")"

let string_of_separated_list (sep : string) (f: 'a -> string) (l : 'a list) : string =
  (String.concat sep (List.map f l))
let string_of_list (f: 'a -> string) (l: 'a list) : string =
  string_of_separated_list ", " f l
let string_of_bounded_list (f: 'a -> string) (lb : string) (rb : string) (l : 'a list) : string =
  lb ^ string_of_list f l ^ rb
let string_of_array (f : 'a -> string) (a: 'a list) =
  string_of_bounded_list f "[" "]" a
let string_of_vec (v: vec) : string =
  string_of_array string_of_float v
let string_of_mat (m: mat) : string = 
  string_of_array string_of_vec m

let rec repeat (s : string) (count : int) : string = 
  if count <= 0 then "" else (if count > 1 then (s ^ (repeat s (count-1))) else s)

let rec transpose (m : 'a list) : 'a list =
  (*https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists*)
  match m with
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let get_mat_square_dim (m : mat) = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    max r c

let string_of_constvar (c : constvar) : string =
  match c with
  | ConstInt i -> string_of_int i
  | ConstVar s -> s

let rec string_of_value (v: value) : string =
  match v with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Float f -> string_of_float f

let string_of_storage_qual (s: storage_qual) : string =
  match s with
  | Const -> "const"
  | InQual -> "in"
  | Out -> "out"
  | Attribute -> "attribute"
  | Uniform -> "uniform"
  | Varying -> "varying"

(*****************************************************
 * Debug-printer
 *****************************************************)

 let debug = false

 let debug_print (s: string) : unit = if debug then Printf.printf "%s\n" s