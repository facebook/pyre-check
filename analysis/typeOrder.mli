(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


exception Cyclic
exception Incomplete
exception Untracked of Type.t


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

(** The handler module for interfacing with TypeOrder lookups. See
    [Environment_handler] for more. *)
module type Handler = sig
  type ('key, 'table) lookup

  val edges: unit -> (int, Target.t list) lookup
  val backedges: unit -> (int, Target.t list) lookup
  val indices: unit -> (Type.t, int) lookup
  val annotations: unit -> (int, Type.t) lookup

  val find: ('key, 'value) lookup -> 'key -> 'value option
  val find_unsafe: ('key, 'value) lookup -> 'key -> 'value
  val contains: ('key, 'value) lookup -> 'key -> bool
  val set: ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val add_key: int -> unit
  val keys: unit -> int list

  val length: ('key, 'value) lookup -> int

  val show: unit -> string
end

(** Provides a default in-process environment handler constructed from a
    [TypeOrder.t]. *)
val handler: t -> (module Handler)

val insert: (module Handler) -> Type.t -> unit
val connect
  :  ?parameters: Type.t list
  -> ?add_backedge: bool
  -> (module Handler)
  -> predecessor: Type.t
  -> successor: Type.t
  -> unit

(* Disconnect the annotation from all of its successors, including any
   backedges. It does not remove the annotation from the TypeOrder. *)
val disconnect_successors: (module Handler) -> Type.t -> unit

(* Returns true if the type order contains the literal annotation. For example, if typing.List
   is in order, `contains order typing.List` will evaluate to true, whereas
   `contains order typing.List[int]` will evaluate to false. *)
val contains: (module Handler) -> Type.t -> bool

(* Returns true if the annotation can be deconstructed into primitive types that exist in the type
   order. If typing.List and int are in the type order, both `contains order typing.List` and
   `is_instantiated order typing.List[int]` will evaluate to true, but
   `is_instantiated order typing.List[str]` will evaluate to false. *)
val is_instantiated: (module Handler) -> Type.t -> bool

val method_resolution_order_linearize
  :  (module Handler)
  -> get_successors:(int -> Target.t list option)
  -> Type.t
  -> Type.t list
val successors: (module Handler) -> Type.t -> Type.t list
val predecessors: (module Handler) -> Type.t -> Type.t list
val greatest: (module Handler) -> matches:(Type.t -> bool) -> Type.t list

val variables: (module Handler) -> Type.t -> Type.t list option

val less_or_equal: (module Handler) -> left: Type.t -> right: Type.t -> bool
val least_upper_bound: (module Handler) -> Type.t -> Type.t -> Type.t list
val greatest_lower_bound: (module Handler) -> Type.t -> Type.t -> Type.t list
val join: (module Handler) -> Type.t -> Type.t -> Type.t
val meet: (module Handler) -> Type.t -> Type.t -> Type.t
val widen
  :  (module Handler)
  -> widening_threshold: int
  -> previous: Type.t
  -> next: Type.t
  -> iteration: int
  -> Type.t
val diff_variables: Type.t Type.Map.t -> Type.t -> Type.t -> Type.t Type.Map.t


val instantiate_successors_parameters
  :  (module Handler)
  -> source: Type.t
  -> target: Type.t
  -> Type.t List.t Option.t

val add_backedges: (module Handler) -> bottom: Type.t -> unit
val deduplicate: (module Handler) -> annotations: Type.t list -> unit
val remove_extra_edges: (module Handler) -> bottom: Type.t -> top: Type.t -> Type.t list -> unit
val connect_annotations_to_top
  :  (module Handler)
  -> top: Type.t
  -> Type.t list

  -> unit

val check_integrity: (module Handler) -> unit

val to_dot: (module Handler) -> string

module Builder: sig
  val create: unit -> t
  val copy: t -> t

  val default: unit -> t
  val builtin_types: Type.Set.t
end
