(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

exception Cyclic

exception Incomplete

exception Untracked of Type.t

module Target : sig
  type t = {
    target: int;
    parameters: Type.OrderedTypes.t;
  }
  [@@deriving compare, eq, sexp, show]

  module type ListOrSet = sig
    type record

    val filter : record -> f:(t -> bool) -> record

    val is_empty : record -> bool

    val exists : record -> f:(t -> bool) -> bool

    val iter : record -> f:(t -> unit) -> unit

    val equal : record -> record -> bool

    val mem : record -> t -> bool

    val to_string : f:(t -> string) -> record -> string

    val fold : record -> init:'accum -> f:('accum -> t -> 'accum) -> 'accum

    val empty : record

    val add : record -> t -> record
  end

  module Set : sig
    include Set.S with type Elt.t = t

    include ListOrSet with type record = t
  end

  module List : ListOrSet with type record = t list
end

type t = {
  edges: Target.t list Int.Table.t;
  backedges: Target.Set.t Int.Table.t;
  indices: int Type.Primitive.Table.t;
  annotations: Type.Primitive.t Int.Table.t;
}
[@@deriving show]

(** The handler module for interfacing with ClassHierarchy lookups. See [Environment_handler] for
    more. *)
module type Handler = sig
  type ('key, 'table) lookup

  val edges : unit -> (int, Target.t list) lookup

  val backedges : unit -> (int, Target.Set.t) lookup

  val indices : unit -> (Type.Primitive.t, int) lookup

  val annotations : unit -> (int, Type.Primitive.t) lookup

  val find : ('key, 'value) lookup -> 'key -> 'value option

  val find_unsafe : ('key, 'value) lookup -> 'key -> 'value

  val contains : ('key, 'value) lookup -> 'key -> bool

  val set : ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val add_key : int -> unit

  val keys : unit -> int list

  val length : ('key, 'value) lookup -> int

  val show : unit -> string
end

val handler : t -> (module Handler)
(** Provides a default in-process environment handler constructed from a [ClassHierarchy.t]. *)

val insert : (module Handler) -> Type.Primitive.t -> unit

val connect
  :  ?parameters:Type.OrderedTypes.t ->
  (module Handler) ->
  predecessor:Type.Primitive.t ->
  successor:Type.Primitive.t ->
  unit

(* Disconnect the annotations from all of its successors, including any backedges. It does not
   remove the annotations from the ClassHierarchy. *)
val disconnect_successors : (module Handler) -> Type.Primitive.t list -> unit

(* Returns true if the class hierarchy contains the given class. *)
val contains : (module Handler) -> Type.Primitive.t -> bool

(* Returns true if the annotation can be deconstructed into primitive types that exist in the class
   hierarchy. If typing.List and int are in the class hierarchy, both `contains hierarchy
   typing.List` and `is_instantiated class hierarchy typing.List[int]` will evaluate to true, but
   `is_instantiated hierarchy typing.List[str]` will evaluate to false. *)
val is_instantiated : (module Handler) -> Type.t -> bool

(* Exposed for tests only *)
val method_resolution_order_linearize
  :  (module Handler) ->
  get_successors:(int -> Target.t list option) ->
  Type.Primitive.t ->
  Type.Primitive.t list

val successors : (module Handler) -> Type.Primitive.t -> Type.Primitive.t list

type variables =
  | Unaries of Type.Variable.Unary.t list
  | ListVariadic of Type.Variable.Variadic.List.t
[@@deriving compare, eq, sexp, show]

val variables
  :  ?default:variables option ->
  (module Handler) ->
  Type.Primitive.t ->
  variables option

val least_upper_bound
  :  (module Handler) ->
  Type.Primitive.t ->
  Type.Primitive.t ->
  Type.Primitive.t list

val greatest_lower_bound
  :  (module Handler) ->
  Type.Primitive.t ->
  Type.Primitive.t ->
  Type.Primitive.t list

val deduplicate : (module Handler) -> annotations:Type.Primitive.t list -> unit

val remove_extra_edges_to_object : (module Handler) -> Type.Primitive.t list -> unit

val connect_annotations_to_object : (module Handler) -> Type.Primitive.t list -> unit

val check_integrity : (module Handler) -> unit

val to_dot : (module Handler) -> string

val is_transitive_successor
  :  (module Handler) ->
  source:Type.Primitive.t ->
  target:Type.Primitive.t ->
  bool

val instantiate_successors_parameters
  :  (module Handler) ->
  source:Type.t ->
  target:Type.Primitive.t ->
  Type.OrderedTypes.t Option.t

val instantiate_predecessors_parameters
  :  (module Handler) ->
  source:Type.t ->
  target:Type.Primitive.t ->
  step:
    (predecessor_variables:Type.OrderedTypes.t ->
    parameters:Type.OrderedTypes.t ->
    TypeConstraints.Solution.t option) ->
  Type.OrderedTypes.t Option.t

module Builder : sig
  val create : unit -> t

  val copy : t -> t

  val builtin_types : Type.Primitive.Set.t

  val add_default_order : (module Handler) -> unit

  val default : unit -> t
end
