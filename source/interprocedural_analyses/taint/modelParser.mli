(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ClassDefinitionsCache : sig
  val invalidate : unit -> unit
end

module ExpectedModel : sig
  type t = {
    model: Model.t;
    target: Interprocedural.Target.t;
    model_source: string;
  }

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val less_or_equal : t -> t -> bool

  val equal : t -> t -> bool
end

(* Exposed for model queries. *)
module Internal : sig
  (* Represents a source or sink kind (e.g, UserControlled) *)
  module Kind : sig
    type t = {
      name: string;
      subkind: string option;
    }
    [@@deriving equal]
  end

  module CollapseDepth : sig
    type t =
      | Value of int
      | Collapse
      | NoCollapse
    [@@deriving equal]
  end

  module TaintFeatures : sig
    type t = {
      breadcrumbs: Features.Breadcrumb.t list;
      via_features: Features.ViaFeature.t list;
      applies_to: Abstract.TreeDomain.Label.path option;
      parameter_path: Abstract.TreeDomain.Label.path option;
      return_path: Abstract.TreeDomain.Label.path option;
      update_path: Abstract.TreeDomain.Label.path option;
      leaf_names: Features.LeafName.t list;
      leaf_name_provided: bool;
      trace_length: int option;
      collapse_depth: CollapseDepth.t option;
    }
    [@@deriving equal]

    val empty : t
  end

  module SanitizeAnnotation : sig
    type t =
      | AllSources
      | SpecificSource of SanitizeTransform.Source.t
      | AllSinks
      | SpecificSink of SanitizeTransform.Sink.t
      | AllTito
      | SpecificTito of {
          sources: SanitizeTransform.Source.t list;
          sinks: SanitizeTransform.Sink.t list;
        }
    [@@deriving equal]
  end

  module TaintAnnotation : sig
    type t =
      | Sink of {
          sink: Sinks.t;
          features: TaintFeatures.t;
        }
      | Source of {
          source: Sources.t;
          features: TaintFeatures.t;
        }
      | Tito of {
          tito: Sinks.t;
          features: TaintFeatures.t;
        }
      | AddFeatureToArgument of { features: TaintFeatures.t }
      | Sanitize of SanitizeAnnotation.t list
    [@@deriving show, equal]

    val from_source : Sources.t -> t

    val from_sink : Sinks.t -> t

    val from_tito : Sinks.t -> t
  end

  module AnnotationKind : sig
    type t =
      | ParameterAnnotation of AccessPath.Root.t
      | ReturnAnnotation
    [@@deriving show, equal]
  end

  module ModelQuery : sig
    module NameConstraint : sig
      type t =
        | Equals of string
        | Matches of Re2.t
      [@@deriving equal, show]
    end

    module AnnotationConstraint : sig
      type t =
        | IsAnnotatedTypeConstraint
        | NameConstraint of NameConstraint.t
      [@@deriving equal, show]
    end

    module ArgumentsConstraint : sig
      type t =
        | Equals of Ast.Expression.Call.Argument.t list
        | Contains of Ast.Expression.Call.Argument.t list
      [@@deriving equal, show]
    end

    module ParameterConstraint : sig
      type t =
        | AnnotationConstraint of AnnotationConstraint.t
        | NameConstraint of NameConstraint.t
        | IndexConstraint of int
        | AnyOf of t list
        | AllOf of t list
        | Not of t
      [@@deriving equal, show]
    end

    module DecoratorConstraint : sig
      type t = {
        name_constraint: NameConstraint.t;
        arguments_constraint: ArgumentsConstraint.t option;
      }
      [@@deriving equal, show]
    end

    module ClassConstraint : sig
      type t =
        | NameConstraint of NameConstraint.t
        | Extends of {
            class_name: string;
            is_transitive: bool;
          }
        | DecoratorConstraint of DecoratorConstraint.t
        | AnyOf of t list
        | AllOf of t list
        | Not of t
        | AnyChildConstraint of {
            class_constraint: t;
            is_transitive: bool;
          }
      [@@deriving equal, show]
    end

    (* An arbitrary constraint for functions, methods, attributes or globals. *)
    module Constraint : sig
      type t =
        | NameConstraint of NameConstraint.t
        | AnnotationConstraint of AnnotationConstraint.t
        | ReturnConstraint of AnnotationConstraint.t
        | AnyParameterConstraint of ParameterConstraint.t
        | AnyOf of t list
        | AllOf of t list
        | ClassConstraint of ClassConstraint.t
        | AnyDecoratorConstraint of DecoratorConstraint.t
        | Not of t
      [@@deriving equal, show]
    end

    module FindKind : sig
      type t =
        | Function
        | Method
        | Attribute
        | Global
      [@@deriving show, equal]
    end

    module QueryTaintAnnotation : sig
      type t =
        | TaintAnnotation of TaintAnnotation.t
        | ParametricSourceFromAnnotation of {
            source_pattern: string;
            kind: string;
          }
        | ParametricSinkFromAnnotation of {
            sink_pattern: string;
            kind: string;
          }
      [@@deriving show, equal]
    end

    module Model : sig
      type t =
        | AllParameters of {
            excludes: string list;
            taint: QueryTaintAnnotation.t list;
          }
        | NamedParameter of {
            name: string;
            taint: QueryTaintAnnotation.t list;
          }
        | PositionalParameter of {
            index: int;
            taint: QueryTaintAnnotation.t list;
          }
        | Parameter of {
            where: ParameterConstraint.t list;
            taint: QueryTaintAnnotation.t list;
          }
        | Return of QueryTaintAnnotation.t list
        | Attribute of QueryTaintAnnotation.t list
        | Global of QueryTaintAnnotation.t list
      [@@deriving show, equal]
    end

    (* `ModelQuery.t` represents a ModelQuery() statement. *)
    type t = {
      location: Ast.Location.t;
      where: Constraint.t list;
      models: Model.t list;
      find: FindKind.t;
      name: string;
      expected_models: ExpectedModel.t list;
      unexpected_models: ExpectedModel.t list;
    }
    [@@deriving show, equal]
  end
end

val get_model_sources : paths:PyrePath.t list -> (PyrePath.t * string) list

module ParseResult : sig
  type t = {
    models: Registry.t;
    queries: Internal.ModelQuery.t list;
    skip_overrides: Ast.Reference.Set.t;
    errors: ModelVerificationError.t list;
  }
end

val parse
  :  resolution:Analysis.Resolution.t ->
  ?path:PyrePath.t ->
  source:string ->
  taint_configuration:TaintConfiguration.Heap.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  callables:Interprocedural.Target.HashSet.t option ->
  stubs:Interprocedural.Target.HashSet.t ->
  unit ->
  ParseResult.t

val verify_model_syntax : path:PyrePath.t -> source:string -> unit

val parse_access_path
  :  path:PyrePath.t option ->
  location:Ast.Location.t ->
  Ast.Expression.t ->
  (Abstract.TreeDomain.Label.path, ModelVerificationError.t) result

(* Exposed for model queries. *)
val create_callable_model_from_annotations
  :  resolution:Analysis.Resolution.t ->
  callable:Interprocedural.Target.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  is_obscure:bool ->
  (Internal.AnnotationKind.t * Internal.TaintAnnotation.t) list ->
  (Model.t, ModelVerificationError.t) result

(* Exposed for model queries. *)
val create_attribute_model_from_annotations
  :  resolution:Analysis.Resolution.t ->
  name:Ast.Reference.t ->
  source_sink_filter:SourceSinkFilter.t option ->
  Internal.TaintAnnotation.t list ->
  (Model.t, ModelVerificationError.t) result
