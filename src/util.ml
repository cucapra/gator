(* Some utilities, from CS6110 assignments *)

(* A hash-based set data structure that behaves like Java's HashSet. *)
module type HashSet = sig
  type 'a t
  val make : unit -> 'a t  (* Create a new set. *)
  val add : 'a t -> 'a -> unit  (* Add something to a set. *)
  val remove : 'a t -> 'a -> unit  (* Remove something from a set. *)
  val mem : 'a t -> 'a -> bool  (* Check whether something exists in a set. *)
  val size : 'a t -> int  (* Get the size of a set. *)
end

module HashSet : HashSet = struct
  type 'a t = ('a, 'a) Hashtbl.t
  let make() : 'a t = Hashtbl.create 16
  let add (h : 'a t) (x : 'a) = Hashtbl.add h x x
  let remove (h : 'a t) (x : 'a) =
    while Hashtbl.mem h x do
      Hashtbl.remove h x
    done
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let size (h : 'a t) : int = Hashtbl.length h
end