(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ClassDefinitionsCache : sig
  val invalidate : unit -> unit
end

(* Exposed for model queries. *)
module Internal : sig
  type breadcrumbs = Features.Breadcrumb.t list [@@deriving show, compare]

  type via_features = Features.ViaFeature.t list [@@deriving show, compare]

  type leaf_kind =
    | Leaf of {
        name: string;
        subkind: string option;
      }
    | Breadcrumbs of breadcrumbs
    | ViaFeatures of via_features
  [@@deriving show, compare]

  type sanitize_annotation =
    | AllSources
    | SpecificSource of Sources.t
    | AllSinks
    | SpecificSink of Sinks.t
    | AllTito
    | SpecificTito of {
        sources: Sources.t list;
        sinks: Sinks.t list;
      }
  [@@deriving show, compare]

  type taint_annotation =
    | Sink of {
        sink: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
        trace_length: int option;
      }
    | Source of {
        source: Sources.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
        trace_length: int option;
      }
    | Tito of {
        tito: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | AddFeatureToArgument of {
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | Sanitize of sanitize_annotation list
  [@@deriving show, compare]

  type annotation_kind =
    | ParameterAnnotation of AccessPath.Root.t
    | ReturnAnnotation
  [@@deriving show, compare]

  module ModelQuery : sig
    type name_constraint =
      | Equals of string
      | Matches of Re2.t
    [@@deriving compare, show]

    type annotation_constraint =
      | IsAnnotatedTypeConstraint
      | AnnotationNameConstraint of name_constraint
    [@@deriving compare, show]

    module ParameterConstraint : sig
      type t =
        | AnnotationConstraint of annotation_constraint
        | NameConstraint of name_constraint
        | IndexConstraint of int
        | AnyOf of t list
        | AllOf of t list
        | Not of t
      [@@deriving compare, show]
    end

    module ArgumentsConstraint : sig
      type t =
        | Equals of Ast.Expression.Call.Argument.t list
        | Contains of Ast.Expression.Call.Argument.t list
      [@@deriving compare, show]
    end

    module DecoratorConstraint : sig
      type t = {
        name_constraint: name_constraint;
        arguments_constraint: ArgumentsConstraint.t option;
      }
      [@@deriving compare, show]
    end

    module ClassConstraint : sig
      type t =
        | NameSatisfies of name_constraint
        | Extends of {
            class_name: string;
            is_transitive: bool;
          }
        | DecoratorSatisfies of DecoratorConstraint.t
      [@@deriving compare, show]
    end

    type model_constraint =
      | NameConstraint of name_constraint
      | AnnotationConstraint of annotation_constraint
      | ReturnConstraint of annotation_constraint
      | AnyParameterConstraint of ParameterConstraint.t
      | AnyOf of model_constraint list
      | AllOf of model_constraint list
      | ParentConstraint of ClassConstraint.t
      | AnyDecoratorConstraint of DecoratorConstraint.t
      | Not of model_constraint
    [@@deriving compare, show]

    type kind =
      | FunctionModel
      | MethodModel
      | AttributeModel
    [@@deriving show, compare]

    type produced_taint =
      | TaintAnnotation of taint_annotation
      | ParametricSourceFromAnnotation of {
          source_pattern: string;
          kind: string;
        }
      | ParametricSinkFromAnnotation of {
          sink_pattern: string;
          kind: string;
        }
    [@@deriving show, compare]

    type production =
      | AllParametersTaint of {
          excludes: string list;
          taint: produced_taint list;
        }
      | NamedParameterTaint of {
          name: string;
          taint: produced_taint list;
        }
      | PositionalParameterTaint of {
          index: int;
          taint: produced_taint list;
        }
      | ParameterTaint of {
          where: ParameterConstraint.t list;
          taint: produced_taint list;
        }
      | ReturnTaint of produced_taint list
      | AttributeTaint of produced_taint list
    [@@deriving show, compare]

    type rule = {
      location: Ast.Location.t;
      query: model_constraint list;
      productions: production list;
      rule_kind: kind;
      name: string;
    }
    [@@deriving show, compare]
  end
end

val get_model_sources : paths:PyrePath.t list -> (PyrePath.t * string) list

type parse_result = {
  models: Registry.t;
  queries: Internal.ModelQuery.rule list;
  skip_overrides: Ast.Reference.Set.t;
  errors: ModelVerificationError.t list;
}

val parse
  :  resolution:Analysis.Resolution.t ->
  ?path:PyrePath.t ->
  ?rule_filter:int list ->
  source:string ->
  configuration:TaintConfiguration.t ->
  callables:Interprocedural.Target.HashSet.t option ->
  stubs:Interprocedural.Target.HashSet.t ->
  unit ->
  parse_result

val verify_model_syntax : path:PyrePath.t -> source:string -> unit

val compute_sources_and_sinks_to_keep
  :  configuration:TaintConfiguration.t ->
  rule_filter:int list option ->
  Sources.Set.t option * Sinks.Set.t option

(* Exposed for model queries. *)
val create_callable_model_from_annotations
  :  resolution:Analysis.Resolution.t ->
  callable:Interprocedural.Target.t ->
  sources_to_keep:Sources.Set.t option ->
  sinks_to_keep:Sinks.Set.t option ->
  is_obscure:bool ->
  (Internal.annotation_kind * Internal.taint_annotation) list ->
  (Model.t, ModelVerificationError.t) result

(* Exposed for model queries. *)
val create_attribute_model_from_annotations
  :  resolution:Analysis.Resolution.t ->
  name:Ast.Reference.t ->
  sources_to_keep:Sources.Set.t option ->
  sinks_to_keep:Sinks.Set.t option ->
  Internal.taint_annotation list ->
  (Model.t, ModelVerificationError.t) result
