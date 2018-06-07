(* Some utilities, from CS6110 assignments *)

(*****************************************************
 * HashSet, like in Java!
 *****************************************************)
module type HashSet = sig
  type 'a t
  val make : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val mem : 'a t -> 'a -> bool
  val size : 'a t -> int
  val values : 'a t -> 'a list
end

module HashSet : HashSet = struct
  type 'a t = ('a, 'a) Hashtbl.t
  let make() : 'a t = Hashtbl.create 16
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let add (h : 'a t) (x : 'a) =
    if mem h x then () else Hashtbl.add h x x
  let remove (h : 'a t) (x : 'a) =
    while Hashtbl.mem h x do
      Hashtbl.remove h x
    done
  let size (h : 'a t) : int = Hashtbl.length h
  let values (h : 'a t) : 'a list =
    Hashtbl.fold (fun x y v -> y :: v) h []
end