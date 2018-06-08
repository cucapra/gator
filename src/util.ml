open Ast
(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string


(*****************************************************
 * HashSet, like in Java!
 *****************************************************)
module type HashSet = sig
  type ('a, 'b) t
  val make : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a * 'b -> unit
  val remove : ('a, 'b) t -> 'a * 'b  -> unit
  val mem : ('a, 'b) t -> 'a -> bool
  val size : ('a, 'b) t -> int
  val values : ('a, 'b) t -> 'a * 'b list
end

module HashSet = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let make() : ('a, 'b) t = Hashtbl.create 16
  let mem (h : ('a, 'b) t) (x : 'a) = Hashtbl.mem h x
  let add (h : ('a, 'b) t) (x : 'a * 'b) =
    if mem h (fst x) then () else Hashtbl.add h (fst x) (snd x)
  let remove (h : ('a, 'b) t) (x : ('a * 'b)) =
    while Hashtbl.mem h (fst x) do
      Hashtbl.remove h (fst x)
    done
  let size (h : ('a, 'b) t) : int = Hashtbl.length h
  let values (h : ('a, 'b) t) : ('a* 'b) list =
    Hashtbl.fold (fun x y v -> (x, y) :: v) h []
  let find (h : ('a, 'b) t) (x: 'a) : 'b = try Hashtbl.find h x
    with Not_found -> raise (ElementNotFoundException "cannot find var in set")
end