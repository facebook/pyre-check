(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

type t =
  | Function of string
  | Method of method_name
  | Override of method_name
  (* Represents a global variable or field of a class that we want to model,
   * e.g os.environ or HttpRequest.GET *)
  | Object of string
[@@deriving sexp, compare, hash, eq]

val property_setter_suffix : string

(* Pretty printers. *)

val pp_internal : Format.formatter -> t -> unit

val show_internal : t -> string

val pp_pretty : Format.formatter -> t -> unit

val show_pretty : t -> string

val pp_pretty_with_kind : Format.formatter -> t -> unit

val show_pretty_with_kind : t -> string

val pp_external : Format.formatter -> t -> unit

val external_name : t -> string

(* Equivalent to pp_internal. Required by @@deriving. *)
val pp : Format.formatter -> t -> unit

(* Constructors. *)

val create_function : Reference.t -> t

val create_method : Reference.t -> t

val create_property_setter : Reference.t -> t

val create_override : Reference.t -> t

val create_property_setter_override : Reference.t -> t

val create_object : Reference.t -> t

val create : Define.t Node.t -> t

val create_derived_override : t -> at_type:Reference.t -> t

(* Accessors. *)

val get_method_reference : t -> Reference.t

val get_override_reference : t -> Reference.t

val get_corresponding_method : t -> t

val get_corresponding_override : t -> t

val class_name : t -> string option

val method_name : t -> string option

(* function or method name, no class or anything else *)
val get_short_name : t -> string

val override_to_method : t -> t

module Map : Core.Map.S with type Key.t = t

module Set : Caml.Set.S with type elt = t

module HashMap : Core.Hashtbl.S with type key := t

module HashSet : Core.Hash_set.S with type elt := t

type definitions_result = {
  qualifier: Reference.t;
  (* Mapping from a target to its selected definition. *)
  callables: Define.t Node.t Map.t;
  (* True if there was multiple non-stub definitions. *)
  has_multiple_definitions: bool;
}

val get_definitions
  :  resolution:Analysis.GlobalResolution.t ->
  Reference.t ->
  definitions_result option

val get_module_and_definition
  :  resolution:Analysis.GlobalResolution.t ->
  t ->
  (Reference.t * Define.t Node.t) option

val resolve_method
  :  resolution:Analysis.GlobalResolution.t ->
  class_type:Type.t ->
  method_name:string ->
  t option

module SharedMemoryKey : sig
  type nonrec t = t

  val compare : t -> t -> int

  val to_string : t -> string

  val from_string : string -> t
end
