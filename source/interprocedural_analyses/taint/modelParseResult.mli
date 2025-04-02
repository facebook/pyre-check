(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ModelParseResult: defines the result of parsing pysa model files (`.pysa`). *)

module AccessPath = Interprocedural.AccessPath

module CollapseDepth : sig
  type t =
    | Value of int
    | Collapse
    | NoCollapse
  [@@deriving equal]
end

module TaintPath : sig
  module Label : sig
    type t =
      | TreeLabel of Abstract.TreeDomain.Label.t
      | ParameterName
    [@@deriving equal, show]
  end

  type t =
    | Path of Label.t list
    | AllStaticFields
  [@@deriving equal, show]

  val has_parameter_name : t -> bool

  val get_access_path : t -> (AccessPath.Path.t, string) result
end

module TaintFeatures : sig
  type t = {
    breadcrumbs: Features.Breadcrumb.t list;
    via_features: Features.ViaFeature.t list;
    applies_to: AccessPath.Path.t option;
    parameter_path: TaintPath.t option;
    return_path: TaintPath.t option;
    update_path: TaintPath.t option;
    leaf_names: Features.LeafName.t list;
    leaf_name_provided: bool;
    trace_length: int option;
    collapse_depth: CollapseDepth.t option;
  }
  [@@deriving equal]

  val empty : t

  val is_empty : t -> bool

  val join : t -> t -> (t, string) Result.t

  val extend_applies_to : t -> Abstract.TreeDomain.Label.t -> t

  val has_path_with_all_static_fields : t -> bool

  val has_path_with_parameter_name : t -> bool
end

module TaintKindsWithFeatures : sig
  type t = {
    kinds: AnnotationParser.KindExpression.t list;
    features: TaintFeatures.t;
  }

  val empty : t

  val from_kinds : AnnotationParser.KindExpression.t list -> t

  val from_kind : AnnotationParser.KindExpression.t -> t

  val from_breadcrumbs : Features.Breadcrumb.t list -> t

  val from_via_features : Features.ViaFeature.t list -> t

  val from_via_feature : Features.ViaFeature.t -> t

  val from_parameter_path : TaintPath.t -> t

  val from_return_path : TaintPath.t -> t

  val from_update_path : TaintPath.t -> t

  val from_collapse_depth : CollapseDepth.t -> t

  val join : t -> t -> (t, string) Result.t

  val concat : t list -> (t, string) Result.t
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

  val add_cross_repository_anchor : canonical_name:string -> canonical_port:string -> t -> t
end

module ModelAnnotation : sig
  type t =
    | ParameterAnnotation of {
        root: AccessPath.Root.t;
        annotation: TaintAnnotation.t;
        (* `generation_if_source = true` means the source is generated in the caller of the modeled
           callable. `generation_if_source = false` means the source is generated in the body of the
           callable, on the given parameter (i.e a "parameter sources"). *)
        generation_if_source: bool;
      }
    | ReturnAnnotation of TaintAnnotation.t
    | ModeAnnotation of Model.ModeSet.t
    | SanitizeAnnotation of Model.Sanitizers.t
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
      | AnnotationClassExtends of {
          class_name: string;
          is_transitive: bool;
          includes_self: bool;
        }
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
      | HasPosition
      | HasName
      | AnyOf of t list
      | AllOf of t list
      | Not of t
    [@@deriving equal, show]
  end

  module DecoratorConstraint : sig
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedCallee of NameConstraint.t
      | ArgumentsConstraint of ArgumentsConstraint.t
      | AnyOf of t list
      | AllOf of t list
      | Not of t
    [@@deriving equal, show]

    val all_of : t list -> t
  end

  module ClassConstraint : sig
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedNameConstraint of NameConstraint.t
      | Extends of {
          class_name: string;
          is_transitive: bool;
          includes_self: bool;
        }
      | DecoratorConstraint of DecoratorConstraint.t
      | AnyOf of t list
      | AllOf of t list
      | Not of t
      | AnyChildConstraint of {
          class_constraint: t;
          is_transitive: bool;
          includes_self: bool;
        }
      | AnyParentConstraint of {
          class_constraint: t;
          is_transitive: bool;
          includes_self: bool;
        }
    [@@deriving equal, show]
  end

  module FormatString : sig
    module IntegerExpression : sig
      type t =
        | Constant of int
        | ParameterPosition
        | Add of {
            left: t;
            right: t;
          }
        | Sub of {
            left: t;
            right: t;
          }
        | Mul of {
            left: t;
            right: t;
          }
      [@@deriving equal, show]
    end

    module Substring : sig
      type t =
        | Literal of string
        | Capture of string
        | FunctionName
        | MethodName
        | ClassName
        | ParameterName
        | Integer of IntegerExpression.t
      [@@deriving equal, show]
    end

    type t = Substring.t list [@@deriving equal, show]
  end

  module ReadFromCache : sig
    type t = {
      kind: string;
      name: string;
    }
    [@@deriving equal, show]
  end

  module WriteToCache : sig
    type t = {
      kind: string;
      name: FormatString.t;
    }
    [@@deriving equal, show]
  end

  (* An arbitrary constraint for functions, methods, attributes or globals. *)
  module Constraint : sig
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedNameConstraint of NameConstraint.t
      | AnnotationConstraint of AnnotationConstraint.t
      | ReturnConstraint of AnnotationConstraint.t
      | AnyParameterConstraint of ParameterConstraint.t
      | ReadFromCache of ReadFromCache.t
      | AnyOf of t list
      | AllOf of t list
      | ClassConstraint of ClassConstraint.t
      | AnyDecoratorConstraint of DecoratorConstraint.t
      | Not of t
    [@@deriving equal, show]

    val contains_read_from_cache : t -> bool

    val is_read_from_cache : t -> bool
  end

  module Find : sig
    type t =
      | Function
      | Method
      | Attribute
      | Global
    [@@deriving show, equal]

    val from_string : string -> t option

    val is_callable : t -> bool

    val is_global : t -> bool

    val is_attribute : t -> bool

    val is_class_member : t -> bool
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
      | CrossRepositoryTaintAnchor of {
          annotation: TaintAnnotation.t;
          canonical_name: FormatString.t;
          canonical_port: FormatString.t;
        }
    [@@deriving show, equal]
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
      | CapturedVariables of {
          taint: QueryTaintAnnotation.t list;
          generation_if_source: bool;
        }
      | Attribute of QueryTaintAnnotation.t list
      | Global of QueryTaintAnnotation.t list
      | Modes of Model.ModeSet.t
      | WriteToCache of WriteToCache.t
    [@@deriving show, equal]

    val is_write_to_cache : t -> bool
  end

  (* `ModelQuery.t` represents a ModelQuery() statement. *)
  type t = {
    location: Ast.Location.t;
    where: Constraint.t list;
    models: Model.t list;
    find: Find.t;
    name: string;
    logging_group_name: string option;
    path: PyrePath.t option;
    expected_models: ExpectedModel.t list;
    unexpected_models: ExpectedModel.t list;
  }
  [@@deriving show, equal]

  val unique_identifier : t -> string

  val extract_extends_from_model_queries : t list -> string list
end

module NameCaptures : sig
  type t

  val create : unit -> t

  val add : t -> Re2.Match.t -> unit

  val get : t -> string -> string option
end

module CallableDecorator : sig
  type t

  val create
    :  pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.Target.CallablesSharedMemory.ReadOnly.t ->
    Ast.Statement.Decorator.t ->
    t

  val create_without_callees : Ast.Statement.Decorator.t -> t

  val statement : t -> Ast.Statement.Decorator.t

  val callees : t -> Interprocedural.CallGraph.CallCallees.t option
end

module Modelable : sig
  type t

  val create_callable
    :  pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.Target.CallablesSharedMemory.ReadOnly.t ->
    Interprocedural.Target.t ->
    t

  val create_attribute
    :  pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t ->
    Interprocedural.Target.t ->
    t

  val create_global
    :  pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t ->
    Interprocedural.Target.t ->
    t

  val target : t -> Interprocedural.Target.t

  val name : t -> Ast.Reference.t

  val type_annotation : t -> Ast.Expression.t option

  val return_annotation : t -> Ast.Expression.t option

  val parameters : t -> Ast.Expression.Parameter.t list

  val decorator_expressions_after_inlining : t -> Ast.Expression.t list

  val resolved_original_decorators : t -> CallableDecorator.t list

  val signature : t -> Interprocedural.Target.CallablesSharedMemory.Signature.t

  val class_name : t -> string option

  val matches_find : t -> ModelQuery.Find.t -> bool

  val expand_format_string
    :  name_captures:NameCaptures.t ->
    parameter:AccessPath.Root.t option ->
    t ->
    ModelQuery.FormatString.t ->
    (string, string) Result.t
end

type t = {
  models: Registry.t;
  queries: ModelQuery.t list;
  errors: ModelVerificationError.t list;
}

val empty : t

val join : t -> t -> t
