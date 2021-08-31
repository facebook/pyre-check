(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A generalization of what can be a call target during the analysis. `callable_t` refers to an
   actual definition. `override_t` is a set of target represented by an override tree. `object_t`
   represents some non-callable object through which taint might flow. *)

open Ast
open Statement

type method_name = {
  class_name: string;
  method_name: string;
}
[@@deriving show, sexp, compare, hash, eq]

type function_t = [ `Function of string ] [@@deriving show, sexp, compare, hash, eq]

type method_t = [ `Method of method_name ] [@@deriving show, sexp, compare, hash, eq]

type callable_t =
  [ function_t
  | method_t
  ]
[@@deriving show, sexp, compare, hash, eq]

type override_t = [ `OverrideTarget of method_name ] [@@deriving show, sexp, compare, hash, eq]

(* Technically not a callable, but we store models of some fields/globals, E.g. os.environ, or
   HttpRequest.GET *)
type object_t = [ `Object of string ] [@@deriving show, sexp, compare, hash, eq]

type non_override_t =
  [ callable_t
  | object_t
  ]
[@@deriving show, sexp, compare, hash, eq]

type t =
  [ non_override_t
  | override_t
  ]
[@@deriving sexp, compare, hash, eq]

val pp : Format.formatter -> t -> unit

val show : [< t ] -> string

val pretty_print : Format.formatter -> [< t ] -> unit

val external_target_name : [< t ] -> string

val class_name : [< t ] -> string option

val compare : ([< t ] as 'a) -> 'a -> int

type t_with_result = callable_t

val create_function : Reference.t -> [> function_t ]

val create_method : Reference.t -> [> method_t ]

val create_property_setter : Reference.t -> [> method_t ]

val create_override : Reference.t -> [> override_t ]

val create_property_setter_override : Reference.t -> [> override_t ]

val create_object : Reference.t -> [> object_t ]

val create : Define.t Node.t -> [> callable_t ]

val create_derived_override : override_t -> at_type:Reference.t -> [> override_t ]

val get_method_reference : method_t -> Reference.t

val get_override_reference : override_t -> Reference.t

val get_corresponding_method : override_t -> [> method_t ]

val get_corresponding_override : method_t -> [> override_t ]

val get_callable_t : [< t ] -> [> callable_t ] option

(* function or method name, no class or anything else *)
val get_short_name : [< t ] -> string

module Key : sig
  type nonrec t = t

  val to_string : t -> string

  val compare : t -> t -> int

  type out = string

  val from_string : string -> out
end

module OverrideKey : sig
  type t = override_t

  val to_string : t -> string

  val compare : t -> t -> int

  type out = string

  val from_string : string -> out
end

module CallableKey : sig
  type t = callable_t

  val to_string : t -> string

  val compare : t -> t -> int

  type out = string

  val from_string : string -> out
end

module Set : Caml.Set.S with type elt = t

val get_module_and_definition
  :  resolution:Analysis.GlobalResolution.t ->
  [< callable_t ] ->
  (Reference.t * Define.t Node.t) option

val resolve_method
  :  resolution:Analysis.GlobalResolution.t ->
  class_type:Type.t ->
  method_name:string ->
  [> method_t ] option

module Map : Core.Map.S with type Key.t = t

module Hashable : Core.Hashable.S with type t := t

module CallableMap : Core.Map.S with type Key.t = callable_t

module CallableSet : Caml.Set.S with type elt = callable_t

module OverrideSet : Caml.Set.S with type elt = override_t

module HashSet : Core.Hash_set.S with type elt := t

module CallableHashSet : Core.Hash_set.S with type elt := callable_t
