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
    parameters: Type.t list
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
  indices: int Type.Table.t;
  annotations: Type.t Int.Table.t
}
[@@deriving show]

(** The handler module for interfacing with TypeOrder lookups. See [Environment_handler] for more. *)
module type Handler = sig
  type ('key, 'table) lookup

  val edges : unit -> (int, Target.t list) lookup

  val backedges : unit -> (int, Target.Set.t) lookup

  val indices : unit -> (Type.t, int) lookup

  val annotations : unit -> (int, Type.t) lookup

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
(** Provides a default in-process environment handler constructed from a [TypeOrder.t]. *)

val insert : (module Handler) -> Type.t -> unit

val connect
  :  ?parameters:Type.t list ->
  (module Handler) ->
  predecessor:Type.t ->
  successor:Type.t ->
  unit

(* Disconnect the annotations from all of its successors, including any backedges. It does not
   remove the annotations from the TypeOrder. *)
val disconnect_successors : (module Handler) -> Type.t list -> unit

(* Returns true if the type order contains the literal annotation. For example, if typing.List is
   in order, `contains order typing.List` will evaluate to true, whereas `contains order
   typing.List[int]` will evaluate to false. *)
val contains : (module Handler) -> Type.t -> bool

(* Returns true if the annotation can be deconstructed into primitive types that exist in the type
   order. If typing.List and int are in the type order, both `contains order typing.List` and
   `is_instantiated order typing.List[int]` will evaluate to true, but `is_instantiated order
   typing.List[str]` will evaluate to false. *)
val is_instantiated : (module Handler) -> Type.t -> bool

val method_resolution_order_linearize
  :  (module Handler) ->
  get_successors:(int -> Target.t list option) ->
  Type.primitive ->
  Type.primitive list

val successors : (module Handler) -> Type.primitive -> Type.primitive list

val variables : (module Handler) -> Type.t -> Type.t list option

module ProtocolAssumptions : sig
  type t

  val empty : t
end
[@@deriving show]

type order = {
  handler: (module Handler);
  constructor: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> Type.t option;
  attributes:
    Type.t -> protocol_assumptions:ProtocolAssumptions.t -> AnnotatedAttribute.t list option;
  is_protocol: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> bool;
  any_is_bottom: bool;
  protocol_assumptions: ProtocolAssumptions.t
}

val solve_less_or_equal
  :  order ->
  constraints:TypeConstraints.t ->
  left:Type.t ->
  right:Type.t ->
  TypeConstraints.t list

val less_or_equal : order -> left:Type.t -> right:Type.t -> bool

val is_compatible_with : order -> left:Type.t -> right:Type.t -> bool

val least_upper_bound : (module Handler) -> Type.t -> Type.t -> Type.t list

val greatest_lower_bound : (module Handler) -> Type.t -> Type.t -> Type.t list

val join : order -> Type.t -> Type.t -> Type.t

val meet : order -> Type.t -> Type.t -> Type.t

val widen
  :  order ->
  widening_threshold:int ->
  previous:Type.t ->
  next:Type.t ->
  iteration:int ->
  Type.t

module OrderedConstraints : TypeConstraints.OrderedConstraintsType with type order = order

val instantiate_protocol_parameters
  :  order ->
  candidate:Type.t ->
  protocol:Ast.Identifier.t ->
  Type.t list option

val instantiate_successors_parameters
  :  order ->
  source:Type.t ->
  target:Type.t ->
  Type.t List.t Option.t

val is_consistent_with : order -> Type.t -> Type.t -> bool

val consistent_solution_exists : order -> Type.t -> Type.t -> bool

val deduplicate : (module Handler) -> annotations:Type.t list -> unit

val remove_extra_edges : (module Handler) -> bottom:Type.t -> top:Type.t -> Type.t list -> unit

val connect_annotations_to_top : (module Handler) -> top:Type.t -> Type.t list -> unit

val sort_bottom_edges : (module Handler) -> bottom:Type.t -> unit

val check_integrity : (module Handler) -> unit

val to_dot : (module Handler) -> string

module Builder : sig
  val create : unit -> t

  val copy : t -> t

  val default : unit -> t

  val builtin_types : Type.Set.t
end
