(** The most general hashtable signature, supporting the four specialisations
    below. *)
module type Hashtbl_generalised = sig
  (** We have three types ([t], [key] and [value]) and three type variables:

      - ['k]/['v] allow the hashtable to determine key/value types;
      - ['a] is carried from keys to corresponding values, allowing the key to
        determine the types of values. *)

  type ('k, 'v) t
  type ('k, 'a) key
  type ('v, 'a) value

  val create : int -> (_, _) t
  val replace : ('k, 'v) t -> ('k, 'a) key -> ('v, 'a) value -> unit
  val remove : ('k, _) t -> ('k, _) key -> unit
  val find_opt : ('k, 'v) t -> ('k, 'a) key -> ('v, 'a) value option

  (* ... *)
end

(** A hashtable with polymorphic keys {i and} values, such as
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stdlib.Hashtbl.html}
    [Stdlib.Hashtbl]}. *)
module type Poly_hash = sig
  (** @inline *)
  include
    Hashtbl_generalised with type ('k, _) key := 'k and type ('v, _) value := 'v
end

(** A hashtable with fixed key type and hashing function, such as
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.S.html}
    [Stdlib.Hashtbl.S]}. *)
module type Mono_hash = sig
  (* The key type is fixed by the implementation functor, so we drop its parameters. *)
  type 'v t
  type key

  (** @inline *)
  include
    Hashtbl_generalised
      with type (_, 'v) t := 'v t
       and type (_, _) key := key
       and type ('v, _) value := 'v
end

(** A hashtable in which bindings are serialised to disk, such as
    {{:https://mirage.github.io/index/index/Index/module-type-S/index.html}
    [Index]}. *)
module type Persistent = sig
  (* All types are fixed by the implementation functor, so we drop all parameters. *)
  type t
  type key
  type value

  (** @inline *)
  include
    Hashtbl_generalised
      with type (_, _) t := t
       and type (_, _) key := key
       and type (_, _) value := value
end

(** A heterogeneous hashtable in which the type of a key determines the type of
    the value that it points to, such as
    {{:https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Core_kernel/Univ_map/}
    [Core_kernel.Univ_map]}. *)
module type Generic = sig
  (* A key determines the type of the value it points to. *)
  type t
  type 'v key

  (** @inline *)
  include
    Hashtbl_generalised
      with type (_, _) t := t
       and type (_, 'a) key := 'a key
       and type (_, 'a) value := 'a
end
