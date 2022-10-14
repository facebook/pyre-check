(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open Statement
module Error = AnalysisError
module TypeResolution = Resolution

module Resolution : sig
  type t

  val of_list : (Reference.t * ReadOnlyness.t) list -> t

  val to_alist : t -> (Reference.t * ReadOnlyness.t) list
end

module Resolved : sig
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module LocalErrorMap : sig
  type t = Error.t list Int.Table.t

  val empty : unit -> t

  val set : statement_key:int -> errors:Error.t list -> t -> unit

  val append : statement_key:int -> error:Error.t -> t -> unit

  val all_errors : t -> Error.t list
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  val error_map : LocalErrorMap.t option

  val local_annotations : LocalAnnotationMap.ReadOnly.t option
end

type callable_data_for_function_call = {
  (* The signature for the function call, after selecting among any overloads. *)
  selected_signature: Type.t Type.Callable.overload;
  (* The return type for the function call, after selecting among any overloads and instantiating
     any type variables. *)
  instantiated_return_type: Type.t;
  function_name: Reference.t option;
  self_readonlyness: ReadOnlyness.t option;
}
[@@deriving compare, show, sexp]

val callable_data_list_for_callee
  :  ?self_readonlyness:ReadOnlyness.t ->
  Type.t ->
  callable_data_for_function_call list

module State (Context : Context) : sig
  open AttributeResolution

  val check_arguments_against_parameters
    :  function_name:Reference.t option ->
    ReadOnlyness.t matched_argument list Type.Callable.Parameter.Map.t ->
    Error.t list

  val forward_expression
    :  type_resolution:TypeResolution.t ->
    resolution:Resolution.t ->
    Expression.t ->
    Resolved.t
end

val readonly_errors_for_define
  :  type_environment:TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  Define.t Node.t ->
  Error.t list
