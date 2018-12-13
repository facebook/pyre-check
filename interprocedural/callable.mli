(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(* A generalization of what can be a call target during the analysis.
   - RealTarget refers to an actual definition.
   - OverrideTarget is a set of target represented by an override tree.
*)

open Ast
open Statement


type method_name = {
  class_name: string;
  method_name: string;
}
[@@deriving show, sexp, compare, hash, eq]

type function_target = [ `Function of string ]
[@@deriving show, sexp, compare, hash, eq]

type method_target = [ `Method of method_name ]
[@@deriving show, sexp, compare, hash, eq]

type real_target = [ function_target | method_target ]
[@@deriving show, sexp, compare, hash, eq]

type override_target = [ `OverrideTarget of method_name ]
[@@deriving show, sexp, compare, hash, eq]

(* Technically not a callable, but we store models of some fields/globals,
   E.g. os.environ, or HttpRequest.GET
*)
type object_target = [ `Object of string ]
[@@deriving show, sexp, compare, hash, eq]

type non_override_target = [ real_target | object_target]
[@@deriving show, sexp, compare, hash, eq]

type t = [ non_override_target | override_target ]
[@@deriving show, sexp, compare, hash, eq]

val show: [< t ] -> string
val external_target_name: [< t ] -> string
val compare: ([< t ] as 'a) -> 'a -> int

type target_with_result = real_target

val create_function: Access.t -> [> function_target ]
val create_method: Access.t -> [> method_target ]
val create_override: Access.t -> [> override_target ]
val create_object: Access.t -> [> object_target ]
val create: Define.t Node.t -> [> real_target ]
val create_derived_override
  : override_target
  -> at_type:Access.t
  -> [> override_target]

val get_method_access: method_target -> Access.t
val get_override_access: override_target -> Access.t
val get_corresponding_method: override_target -> [> method_target]
val get_corresponding_override: method_target -> [> override_target]

module Key : sig
  type nonrec t = t
  val to_string: t -> string
  val compare: t -> t -> int
end

module OverrideKey : sig
  type t = override_target
  val to_string: t -> string
  val compare: t -> t -> int
end

module RealKey : sig
  type t = real_target
  val to_string: t -> string
  val compare: t -> t -> int
end

module Set : Caml.Set.S with type elt = t

(* Shared heap access to top-level definitions. *)
val add_function_definition: Access.t -> File.Handle.t -> unit
val add_class_definition: Access.t -> File.Handle.t -> unit
val get_definition: [< real_target ] -> Define.t Node.t option


module Map : Core.Map.S with type Key.t = t
module Hashable : Core.Hashable.S with type t := t
module RealMap : Core.Map.S with type Key.t = real_target
module OverrideSet : Caml.Set.S with type elt = override_target
