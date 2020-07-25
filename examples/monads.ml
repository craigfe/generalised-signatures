(** We begin with the classic monad signature, with a single type parameter: *)

module type Monad = sig
  type _ t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** This signature is enough to include many sensible monad instances.
    Unfortunately, it can't be used to describe types that are monadic only when
    {i partially applied}, like {!result}.

    We could define a {i name} for the partially-applied operator that
    specialises {!result} to a particular [error] type:

    {[
      (* Variant type holding the set of possible program errors *)
      type error = ..
      type 'a or_error = ('a, error) result
    ]}

    but life is too short to have each program define its own monad instance for
    its specific error cases. Instead, we can extend our {!Monad} signature to
    carry an extra type parameter, held constant over each occurrence of [t],
    that {!result} can use to hold its error type. Since this extra type
    parameter isn't meaningful to the monad interface, we can consider it a
    {i phantom} type parameter ['p].

    This gives the following signature: *)

module type Monad_phantom = sig
  type (_, _) t

  val return : 'a -> ('a, 'p) t
  val bind : ('a, 'p) t -> ('a -> ('b, 'p) t) -> ('b, 'p) t
end

(** Now we can provide a {!result} instance for our monad signature: *)

module Result_phantom : Monad_phantom with type ('o, 'e) t := ('o, 'e) result =
struct
  let return x = Ok x
  let bind = function Ok x -> fun f -> f x | Error _ as e -> Fun.const e
end

(** We can go even further in adding type-level features to our monad signature.
    Some standard monad instances are like {!Result} in being parameterised over
    some type [p], but take this a step further by allowing [p] to {i change}
    throughout the computation.

    Consider the {i state} monad, which threads a value of some type throughout
    a computation: *)

type ('a, 's) state = 's -> 'a * 's

(** If we allow computations to change this type, then we'll need both an
    {i input} and an {i output} state type for each computation: *)

type ('a, -'input, +'output) indexed_state = 'input -> 'a * 'output

(** Now when we sequence computations with [( >>= )] we just need to connect the
    inputs up with the outputs:

    {[
      val bind :
        ('a, 'input, 'middle) t ->
        ('a -> ('b, 'middle, 'output) t) ->
        ('b, 'input, 'output) t
    ]}

    What we've described is sometimes called Atkey's
    {{:http://bentnib.org/paramnotions-jfp.pdf} {i indexed monad}}, and its
    interface captures even more sensible monad instances: *)

module type Monad_indexed = sig
  type (_, +_, -_) t

  val return : 'a -> ('a, 'i, 'i) t
  val bind : ('a, 'i, 'j) t -> ('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t
end

(** Now that we've pulled together some different type-level implementations of
    monads, we can write a generalisation of them: *)

module type Monad_generalised = sig
  type (_, +_, -_, _) t

  val return : 'a -> ('a, 'i, 'i, 'p) t

  val bind :
    ('a, 'i, 'j, 'p) t -> ('a -> ('b, 'j, 'k, 'p) t) -> ('b, 'i, 'k, 'p) t
end

(** From this, we can easily recover each of our monad variants: *)

module type Monad_simple' = sig
  type 'a t

  include Monad_generalised with type ('a, _, _, _) t := 'a t
end

module type Monad_indexed' = sig
  type ('a, +'i, -'j) t

  include Monad_generalised with type ('a, 'i, 'j, _) t := ('a, 'i, 'j) t
end

module type Monad_phantom' = sig
  type ('a, 'p) t

  include Monad_generalised with type ('a, _, _, 'p) t := ('a, 'p) t
end

(** {!Monad_generalised} admits the common monad instances: *)

module Identity : Monad_generalised with type ('a, _, _, _) t := 'a = struct
  let return x = x
  let bind x f = f x
end

module List : Monad_generalised with type ('a, _, _, _) t := 'a list = struct
  let return x = [ x ]
  let bind x f = List.concat_map f x
end

module Result :
  Monad_generalised with type ('a, _, _, 'e) t := ('a, 'e) result = struct
  let return x = Ok x
  let bind = Result.bind
end

module Reader : Monad_generalised with type ('a, _, _, 's) t := 's -> 'a =
struct
  let return x _ = x
  let bind f g x = g (f x) x
end

(** Both forms of the state monad (indexed and non-indexed) add extra [get] and
    [put] operations to the monad interface: *)

module type State_generalised = sig
  include Monad_indexed'

  type 'a s

  val get : ('a s, 'a s, 'a s) t
  val put : 'j s -> (unit, 'i s, 'j s) t
end

(** Once again, we can recover the less general signatures with destructive
    substitution: *)

module type State_fixed' = sig
  type 'a t
  type s

  include State_generalised with type ('a, _, _) t := 'a t with type 'a s := s
end

module type State_indexed' = sig
  type ('a, +'i, -'j) t

  include
    State_generalised
      with type ('a, 'i, 'j) t := ('a, 'i, 'j) t
       and type 'a s := 'a
end
