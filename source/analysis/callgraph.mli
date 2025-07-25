(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type dispatch =
  | Dynamic
  | Static

and callee =
  | Function of Reference.t
  | Method of {
      class_name: Type.t;
      direct_target: Reference.t;
      dispatch: dispatch;
      is_optional_class_attribute: bool;
    }
  | PropertySetter of {
      class_name: Type.t;
      direct_target: Reference.t;
    }
[@@deriving sexp, compare, equal, show]

type callee_with_locations = {
  callee: callee;
  locations: Location.WithModule.t list;
}
[@@deriving equal]

val callee_to_yojson : ?locations:Location.WithPath.t list -> callee -> Yojson.Safe.t

module type Builder = sig
  val initialize : unit -> unit

  val add_callee
    :  global_resolution:GlobalResolution.t ->
    target:Type.t option ->
    callables:Type.Callable.t list ->
    dynamic:bool ->
    qualifier:Reference.t ->
    callee_type:Type.t ->
    callee:Ast.Expression.t ->
    unit

  val add_property_callees
    :  global_resolution:GlobalResolution.t ->
    resolved_base:Type.t ->
    attributes:(AnnotatedAttribute.instantiated * Type.t) list ->
    name:string ->
    qualifier:Reference.t ->
    location:Ast.Location.t ->
    unit

  val add_property_setter_callees
    :  attribute:AnnotatedAttribute.instantiated ->
    instantiated_parent:Type.t ->
    name:string ->
    location:Location.WithModule.t ->
    unit

  val get_all_callees : unit -> callee_with_locations list
end

module DefaultBuilder : Builder

module NullBuilder : Builder
