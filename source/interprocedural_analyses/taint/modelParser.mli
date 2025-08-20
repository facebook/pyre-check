(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module PyrePysaLogic = Analysis.PyrePysaLogic

module PythonVersion : sig
  include module type of Configuration.PythonVersion

  val parse_from_tuple : Ast.Expression.Expression.t list -> (t, string) result

  val from_configuration : Configuration.Analysis.t -> t

  val compare_with
    :  t ->
    Ast.Expression.ComparisonOperator.operator ->
    t ->
    (bool, Ast.Expression.ComparisonOperator.operator) result
end

val get_model_sources : paths:PyrePath.t list -> (PyrePath.t * string) list

val parse
  :  pyre_api:Interprocedural.PyrePysaApi.ReadOnly.t ->
  ?path:PyrePath.t ->
  source:string ->
  taint_configuration:TaintConfiguration.Heap.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  definitions:Interprocedural.Target.HashSet.t option ->
  stubs:Interprocedural.Target.HashsetSharedMemory.ReadOnly.t ->
  python_version:PythonVersion.t ->
  unit ->
  ModelParseResult.t

val verify_model_syntax : path:PyrePath.t -> source:string -> unit

val parse_access_path
  :  path:PyrePath.t option ->
  location:Ast.Location.t ->
  Ast.Expression.t ->
  (ModelParseResult.TaintPath.t, ModelVerificationError.t) result

val parse_model_modes
  :  path:PyrePath.t ->
  source:string ->
  Model.ModeSet.t Ast.Reference.SerializableMap.t

val decorator_actions_from_modes
  :  Model.ModeSet.t Ast.Reference.SerializableMap.t ->
  PyrePysaLogic.DecoratorPreprocessing.Action.t Ast.Reference.SerializableMap.t

(* Exposed for testing purpose *)
val demangle_class_attribute : Ast.Reference.t -> Ast.Reference.t

(* Exposed for model queries. *)
val create_callable_model_from_annotations
  :  pyre_api:Interprocedural.PyrePysaApi.ReadOnly.t ->
  modelable:ModelParseResult.Modelable.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  is_obscure:bool ->
  ModelParseResult.ModelAnnotation.t list ->
  (Model.t, ModelVerificationError.t) result

(* Exposed for model queries. *)
val create_attribute_model_from_annotations
  :  pyre_api:Interprocedural.PyrePysaApi.ReadOnly.t ->
  name:Ast.Reference.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  ModelParseResult.TaintAnnotation.t list ->
  (Model.t, ModelVerificationError.t) result
