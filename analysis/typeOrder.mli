(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

exception Cyclic
exception Incomplete
exception Undefined of Type.t

module Target: sig
  type t = {
    target: int;
    parameters: Type.t list
  }
  [@@deriving compare, eq, sexp, show]

  module Set: Set.S with type Elt.t = t
end

module Edge : sig
  type t = {
    source: Type.t;
    target: Type.t;
  }
  [@@deriving compare, eq, sexp, show]

  module Set: Set.S with type Elt.t = t
end

type t = {
  edges: (Target.t list) Int.Table.t;
  backedges: (Target.t list) Int.Table.t;
  indices: int Type.Table.t;
  annotations: Type.t Int.Table.t;
}
[@@deriving show]

(** The reader module for interfacing with TypeOrder lookups. See
    [Environment_reader] for more. *)
module type Reader = sig
  type ('key, 'table) lookup

  val edges: unit -> (int, Target.t list) lookup
  val backedges: unit -> (int, Target.t list) lookup
  val indices: unit -> (Type.t, int) lookup
  val annotations: unit -> (int, Type.t) lookup

  val find: ('key, 'value) lookup -> 'key -> 'value option
  val find_unsafe: ('key, 'value) lookup -> 'key -> 'value
  val contains: ('key, 'value) lookup -> 'key -> bool
  val set: ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val fold
    :  ('key, 'value) lookup
    -> init:'accumulator
    -> f:(key:'key -> data:'value -> 'accumulator -> 'accumulator)
    -> 'accumulator

  val keys: ('key, 'value) lookup -> 'key list
  val length: ('key, 'value) lookup -> int

  val show: unit -> string
end

(** Provides a default in-process environment reader constructed from a
    [TypeOrder.t]. *)
val reader: t -> (module Reader)

val insert: (module Reader) -> Type.t -> unit
val connect
  :  ?parameters: Type.t list
  -> (module Reader)
  -> predecessor: Type.t
  -> successor: Type.t
  -> unit
val find: (module Reader) -> Type.t -> Type.t option
val contains: (module Reader) -> Type.t -> bool

val successors_fold
  :  (module Reader)
  -> initial: 'accumulator
  -> f: ('accumulator -> Type.t -> 'accumulator)
  -> Type.t
  -> 'accumulator
val successors: (module Reader) -> Type.t -> Type.t list
val predecessors: (module Reader) -> Type.t -> Type.t list
val greatest: (module Reader) -> matches:(Type.t -> bool) -> Type.t list

val less_or_equal: (module Reader) -> left:Type.t -> right:Type.t -> bool
val least_upper_bound: (module Reader) -> Type.t -> Type.t -> Type.t list
val greatest_lower_bound: (module Reader) -> Type.t -> Type.t -> Type.t list
val join: (module Reader) -> Type.t -> Type.t -> Type.t
val meet: (module Reader) -> Type.t -> Type.t -> Type.t
val widen
  :  (module Reader)
  -> widening_threshold: int
  -> previous: Type.t
  -> next: Type.t
  -> iteration: int
  -> Type.t

val instantiate_parameters
  :  (module Reader)
  -> source:Type.t
  -> target:Type.t
  -> Type.t List.t Option.t

val complete: (module Reader) -> bottom: Type.t -> top: Type.t -> unit
val check_integrity: (module Reader) -> unit

module Builder: sig
  val create: unit -> t
  val copy: t -> t

  val default: unit -> t
end
