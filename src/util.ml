open CoreAst

(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string

let flip f x y = f y x
let compose f g x = f (g x)
let ( |- ) = compose
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let tr_fst ((x, _, _) : 'a * 'b * 'c) : 'a = x
let tr_snd ((_, x, _) : 'a * 'b * 'c) : 'b = x
let tr_thd ((_, _, x) : 'a * 'b * 'c) : 'c = x
let nonempty x = List.length x > 0
let string_of_pair (a : string) (b : string) : string = "(" ^ a ^ ", " ^ b ^ ")"
let contains l v = List.fold_left (fun acc x -> (x = v) || acc) false l

let string_of_separated_list (sep : string) (f : 'a -> string) (l : 'a list) :
    string =
  String.concat sep (List.map f l)

let string_of_list (f : 'a -> string) (l : 'a list) : string =
  string_of_separated_list ", " f l

let string_of_bounded_list (f : 'a -> string) (lb : string) (rb : string)
    (l : 'a list) : string =
  lb ^ string_of_list f l ^ rb

let string_of_array (f : 'a -> string) (a : 'a list) =
  string_of_bounded_list f "[" "]" a

let list_replace (x : 'a) (l : 'a list) (index : int) =
  if index > List.length l || index < 0 then failwith "Index out of range"
  else List.mapi (fun i y -> if i == index then x else y) l

let rec repeat (s : string) (count : int) : string =
  if count <= 0 then "" else if count > 1 then s ^ repeat s (count - 1) else s

let rec transpose (m : 'a list) : 'a list =
  (*https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists*)
  match m with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let get_mat_square_dim (m : float list list) =
  (* Note the transpose to match the glsl column-oriented style *)
  let tm = transpose m in
  let r = List.length tm in
  let c = if r = 0 then 0 else List.length (List.hd tm) in
  max r c

let string_of_constvar (c : constvar) : string =
  match c with ConstInt i -> string_of_int i | ConstVar s -> s

let rec string_of_value (v : value) : string =
  match v with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Float f -> string_of_float f
  | StringVal s -> "\"" ^ s ^ "\""

let string_of_storage_qual (s : storage_qual) : string =
  match s with
  | Const -> "const"
  | InQual -> "in"
  | Out -> "out"
  | Attribute -> "attribute"
  | Uniform -> "uniform"
  | Varying -> "varying"
  | BuiltIn -> ""

(*****************************************************
 * Debug-printer
 *****************************************************)

let debug = ref true
let pretty_printer = ref true

(*Add pretty printing flag here*)

let debug_print (s : string) : unit = if !debug then Printf.printf "%s\n" s
