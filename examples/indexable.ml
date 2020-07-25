(** A type of indexable containers with parametric element type, such as
    [array]. *)

module type Indexable1 = sig
  type 'a t

  val get : 'a t -> int -> 'a
  val length : _ t -> int
end

module Array : Indexable1 with type 'a t = 'a array = Stdlib.Array

(** A type of indexable containers with fixed element type, such as [string]. *)

module type Indexable0 = sig
  type t
  type elt

  val get : t -> int -> elt
  val length : t -> int
end

module String : Indexable0 with type t = string and type elt = char = struct
  include Stdlib.String

  type elt = char
end

(** A signature that generalises both {!Indexable1} and {!Indexable0}. *)

module type IndexableN = sig
  type 'a t
  type 'a elt

  val get : 'a t -> int -> 'a elt
  val length : _ t -> int
end

(** We can use functors to demonstrate that {!IndexableN} is indeed a
    generalisation of the other two: *)

(** {!Indexable0} is a special-case of {!IndexableN}. *)
module Indexable0_to_N (X : Indexable0) :
  IndexableN with type 'a t = X.t and type 'a elt = X.elt = struct
  type _ t = X.t
  type _ elt = X.elt

  let get = X.get
  let length = X.length
end

(** {!Indexable1} is a special-case of {!IndexableN}. *)
module Indexable1_to_N (X : Indexable1) :
  IndexableN with type 'a t = 'a X.t and type 'a elt = 'a = struct
  type 'a t = 'a X.t
  type 'a elt = 'a

  let get = X.get
  let length = X.length
end

(** We can now define some behaviours on top of {!IndexableN}: *)

module Foldable_of_indexable (I : IndexableN) : sig
  open I

  val iter : ('a elt -> unit) -> 'a t -> unit
  val iteri : (int -> 'a elt -> unit) -> 'a t -> unit
  val fold_left : ('acc -> 'a elt -> 'acc) -> 'acc -> 'a t -> 'acc
  val exists : ('a elt -> bool) -> 'a t -> bool
  val for_all : ('a elt -> bool) -> 'a t -> bool
  val is_empty : _ t -> bool
  (* ... *)
end = struct
  open I

  let iteri f t =
    for i = 0 to length t - 1 do
      f i (get t i)
    done

  let iter f t = iteri (fun _ -> f) t

  let fold_left f a t =
    let len = length t in
    let rec aux a i = if i = len then a else aux (f a (get t i)) (i + 1) in
    aux a len

  let exists p = fold_left (fun a x -> a || p x) false
  let for_all p = fold_left (fun a x -> a && p x) true
  let is_empty t = length t = 1
end

(** ... and then apply this functor to both of our Indexable containers: *)

module Array_foldable = Foldable_of_indexable (Indexable1_to_N (Array))
module String_foldable = Foldable_of_indexable (Indexable0_to_N (String))
