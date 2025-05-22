(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
module Error = AnalysisError

module LocalErrorMap : sig
  type t

  val empty : unit -> t

  val all_errors : t -> Error.t list
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val no_validation_on_class_lookup_failure : bool

  val define : Define.t Node.t

  (* Where to store local annotations during the fixpoint. `None` discards them. *)
  val resolution_fixpoint : TypeInfo.ForFunctionBody.t option

  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option

  module Builder : Callgraph.Builder

  val record_expression_type_info : Expression.t -> TypeInfo.Unit.t -> unit
end

module type Signature = sig
  type t [@@deriving eq]

  val create : resolution:Resolution.t -> t

  val unreachable : t

  val resolution : t -> Resolution.t option

  val initial : resolution:Resolution.t -> t

  val parse_and_check_annotation
    :  ?bind_variables:bool ->
    resolution:Resolution.t ->
    Expression.t ->
    Error.t list * Type.t

  include Fixpoint.State with type t := t
end

val unpack_callable_and_self_argument
  :  signature_select:
       (arguments:Type.t SignatureSelection.Argument.t list ->
       callable:Type.Callable.t ->
       self_argument:Type.t option ->
       SignatureSelectionTypes.instantiated_return_annotation) ->
  global_resolution:GlobalResolution.t ->
  Type.t ->
  TypeOperation.callable_and_self_argument option

module State (_ : Context) : Signature

module DummyContext : Context

val resolution
  :  GlobalResolution.t ->
  ?type_info_store:TypeInfo.Store.t ->
  (module Context) ->
  Resolution.t

val resolution_at_key
  :  global_resolution:GlobalResolution.t ->
  local_annotations:TypeInfo.ForFunctionBody.ReadOnly.t option ->
  parent:Reference.t option ->
  statement_key:int ->
  (module Context) ->
  Resolution.t

val compute_local_annotations
  :  type_check_controls:EnvironmentControls.TypeCheckControls.t ->
  global_resolution:GlobalResolution.t ->
  define_name:Reference.t ->
  define_location:Location.t ->
  (TypeInfo.ForFunctionBody.ReadOnly.t * (Expression.t * TypeInfo.Unit.t) list Location.Table.t)
  option

module CheckResult : sig
  type t = {
    errors: Error.t list option;
    local_annotations: TypeInfo.ForFunctionBody.ReadOnly.t Location.SerializableMap.t;
    callees: Callgraph.callee_with_locations list option;
  }
  [@@deriving equal]

  val errors : t -> Error.t list option

  val local_annotations
    :  t ->
    define_location:Location.t ->
    TypeInfo.ForFunctionBody.ReadOnly.t option

  val callees : t -> Callgraph.callee_with_locations list option
end

val check_define_by_name
  :  type_check_controls:EnvironmentControls.TypeCheckControls.t ->
  call_graph_builder:(module Callgraph.Builder) ->
  global_environment:AnnotatedGlobalEnvironment.ReadOnly.t ->
  dependency:SharedMemoryKeys.DependencyKey.registered option ->
  Ast.Reference.t ->
  CheckResult.t option
