(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* A generalization of what can be a call target during the analysis. - RealTarget refers to an
   actual definition. - OverrideTarget is a set of target represented by an override tree. *)

open Ast
open Statement

type method_name = {
  class_name: string;
  method_name: string;
}
[@@deriving show, sexp, compare, hash, eq]

type function_target = [ `Function of string ] [@@deriving show, sexp, compare, hash, eq]

type method_target = [ `Method of method_name ] [@@deriving show, sexp, compare, hash, eq]

type real_target =
  [ function_target
  | method_target
  ]
[@@deriving show, sexp, compare, hash, eq]

type override_target = [ `OverrideTarget of method_name ]
[@@deriving show, sexp, compare, hash, eq]

(* Technically not a callable, but we store models of some fields/globals, E.g. os.environ, or
   HttpRequest.GET *)
type object_target = [ `Object of string ] [@@deriving show, sexp, compare, hash, eq]

type non_override_target =
  [ real_target
  | object_target
  ]
[@@deriving show, sexp, compare, hash, eq]

type t =
  [ non_override_target
  | override_target
  ]
[@@deriving sexp, compare, hash, eq]

val pp : Format.formatter -> t -> unit

val show : [< t ] -> string

val pretty_print : Format.formatter -> [< t ] -> unit

val external_target_name : [< t ] -> string

val compare : ([< t ] as 'a) -> 'a -> int

type target_with_result = real_target

val create_function : Reference.t -> [> function_target ]

val create_method : Reference.t -> [> method_target ]

val create_override : Reference.t -> [> override_target ]

val create_object : Reference.t -> [> object_target ]

val create : Define.t Node.t -> [> real_target ]

val create_derived_override : override_target -> at_type:Reference.t -> [> override_target ]

val get_method_reference : method_target -> Reference.t

val get_override_reference : override_target -> Reference.t

val get_corresponding_method : override_target -> [> method_target ]

val get_corresponding_override : method_target -> [> override_target ]

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
  type t = override_target

  val to_string : t -> string

  val compare : t -> t -> int

  type out = string

  val from_string : string -> out
end

module RealKey : sig
  type t = real_target

  val to_string : t -> string

  val compare : t -> t -> int

  type out = string

  val from_string : string -> out
end

module Set : Caml.Set.S with type elt = t

val get_definition
  :  resolution:Analysis.GlobalResolution.t ->
  [< real_target ] ->
  Define.t Node.t option

val get_method_implementation
  :  resolution:Analysis.GlobalResolution.t ->
  class_type:Type.t ->
  method_name:Reference.t ->
  [> method_target ] option

module Map : Core.Map.S with type Key.t = t

module Hashable : Core.Hashable.S with type t := t

module RealMap : Core.Map.S with type Key.t = real_target

module OverrideSet : Caml.Set.S with type elt = override_target
