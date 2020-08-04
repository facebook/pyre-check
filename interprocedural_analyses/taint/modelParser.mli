(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre

module ClassDefinitionsCache : sig
  val invalidate : unit -> unit
end

module T : sig
  type parse_result = {
    models: TaintResult.call_model Interprocedural.Callable.Map.t;
    skip_overrides: Ast.Reference.Set.t;
    errors: string list;
  }

  type breadcrumbs = Features.Simple.t list

  type leaf_kind =
    | Leaf of string
    | Breadcrumbs of breadcrumbs

  type taint_annotation =
    | Sink of {
        sink: Sinks.t;
        breadcrumbs: breadcrumbs;
        path: Abstract.TreeDomain.Label.path;
        leaf_name_provided: bool;
      }
    | Source of {
        source: Sources.t;
        breadcrumbs: breadcrumbs;
        path: Abstract.TreeDomain.Label.path;
        leaf_name_provided: bool;
      }
    | Tito of {
        tito: Sinks.t;
        breadcrumbs: breadcrumbs;
        path: Abstract.TreeDomain.Label.path;
      }
    | AddFeatureToArgument of {
        breadcrumbs: breadcrumbs;
        path: Abstract.TreeDomain.Label.path;
      }
    | SkipAnalysis (* Don't analyze methods with SkipAnalysis *)
    | SkipOverrides (* Analyze as normally, but assume no overrides exist. *)
    | Sanitize

  type annotation_kind =
    | ParameterAnnotation of AccessPath.Root.t
    | ReturnAnnotation
end

val parse
  :  resolution:Analysis.Resolution.t ->
  ?path:Path.t ->
  ?rule_filter:int list ->
  source:string ->
  configuration:Configuration.t ->
  TaintResult.call_model Interprocedural.Callable.Map.t ->
  T.parse_result

val verify_model_syntax : path:Path.t -> source:string -> unit

val add_taint_annotation_to_model
  :  resolution:Analysis.GlobalResolution.t ->
  annotation_kind:T.annotation_kind ->
  callable_annotation:Type.t Type.Callable.record option ->
  sources_to_keep:Sources.Set.t option ->
  sinks_to_keep:Sinks.Set.t option ->
  name:string ->
  TaintResult.call_model * string option ->
  T.taint_annotation ->
  TaintResult.call_model * string option
