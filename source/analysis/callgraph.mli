(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
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
[@@deriving sexp, compare, eq, show]

type callee_with_locations = {
  callee: callee;
  locations: Location.WithModule.t list;
}

val callee_to_yojson : ?locations:Location.WithPath.t list -> callee -> Yojson.Safe.t

type caller =
  | FunctionCaller of Reference.t
  | PropertySetterCaller of Reference.t
[@@deriving compare, sexp]

module CalleeValue : Memory.ValueType with type t = callee_with_locations list

module CallerKey : Memory.KeyType with type t = caller

module SharedMemory :
  Memory.WithCache.S
    with type value = CalleeValue.t
     and type key = caller
     and module KeySet = Caml.Set.Make(CallerKey)
     and module KeyMap = MyMap.Make(CallerKey)

val set : caller:caller -> callees:callee_with_locations list -> unit

val get : caller:caller -> callee_with_locations list

module type Builder = sig
  val initialize : unit -> unit

  val add_callee
    :  global_resolution:GlobalResolution.t ->
    target:Type.t option ->
    callables:Type.Callable.t list ->
    arguments:Expression.Call.Argument.t list ->
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
