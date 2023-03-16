(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_model_sources : paths:PyrePath.t list -> (PyrePath.t * string) list

val parse
  :  resolution:Analysis.GlobalResolution.t ->
  ?path:PyrePath.t ->
  source:string ->
  taint_configuration:TaintConfiguration.Heap.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  callables:Interprocedural.Target.HashSet.t option ->
  stubs:Interprocedural.Target.HashSet.t ->
  unit ->
  ModelParseResult.t

val verify_model_syntax : path:PyrePath.t -> source:string -> unit

val parse_access_path
  :  path:PyrePath.t option ->
  location:Ast.Location.t ->
  Ast.Expression.t ->
  (Abstract.TreeDomain.Label.path, ModelVerificationError.t) result

val parse_decorators_to_skip_when_inlining
  :  path:PyrePath.t ->
  source:string ->
  Ast.Reference.t list

(* Exposed for model queries. *)
val create_callable_model_from_annotations
  :  resolution:Analysis.GlobalResolution.t ->
  callable:Interprocedural.Target.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  is_obscure:bool ->
  ModelParseResult.ModelAnnotation.t list ->
  (Model.t, ModelVerificationError.t) result

(* Exposed for model queries. *)
val create_attribute_model_from_annotations
  :  resolution:Analysis.GlobalResolution.t ->
  name:Ast.Reference.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  ModelParseResult.TaintAnnotation.t list ->
  (Model.t, ModelVerificationError.t) result
