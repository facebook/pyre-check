(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ModelParseResult: defines the result of parsing pysa model files (`.pysa`). *)

(* Represents a source or sink kind (e.g, UserControlled) *)
module Kind : sig
  type t = {
    name: string;
    subkind: string option;
  }
  [@@deriving equal]

  val from_name : string -> t
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

  val is_empty : t -> bool

  val join : t -> t -> (t, string) Result.t

  val extend_applies_to : t -> Abstract.TreeDomain.Label.t -> t
end

module TaintKindsWithFeatures : sig
  type t = {
    kinds: Kind.t list;
    features: TaintFeatures.t;
  }

  val empty : t

  val from_kinds : Kind.t list -> t

  val from_kind : Kind.t -> t

  val from_breadcrumbs : Features.Breadcrumb.t list -> t

  val from_via_features : Features.ViaFeature.t list -> t

  val from_via_feature : Features.ViaFeature.t -> t

  val from_parameter_path : Abstract.TreeDomain.Label.path -> t

  val from_return_path : Abstract.TreeDomain.Label.path -> t

  val from_update_path : Abstract.TreeDomain.Label.path -> t

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
end

module ModelAnnotation : sig
  type t =
    | ParameterAnnotation of AccessPath.Root.t * TaintAnnotation.t
    | ReturnAnnotation of TaintAnnotation.t
    | ModeAnnotation of Model.ModeSet.t
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
      | AnyOf of t list
      | AllOf of t list
      | Not of t
    [@@deriving equal, show]
  end

  module DecoratorConstraint : sig
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedNameConstraint of NameConstraint.t
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

  module ReadFromCache : sig
    type t = {
      kind: string;
      name: string;
    }
    [@@deriving equal, show]
  end

  module WriteToCache : sig
    module Substring : sig
      type t =
        | Literal of string
        | Capture of string
        | FunctionName
        | MethodName
        | ClassName
      [@@deriving equal, show]
    end

    type t = {
      kind: string;
      name: Substring.t list;
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

module Modelable : sig
  type t =
    | Callable of {
        target: Interprocedural.Target.t;
        signature: Ast.Statement.Define.Signature.t lazy_t;
      }
    | Attribute of {
        name: Ast.Reference.t;
        type_annotation: Ast.Expression.t option lazy_t;
      }
    | Global of {
        name: Ast.Reference.t;
        type_annotation: Ast.Expression.t option lazy_t;
      }

  val target : t -> Interprocedural.Target.t

  val name : t -> Ast.Reference.t

  val type_annotation : t -> Ast.Expression.t option

  val return_annotation : t -> Ast.Expression.t option

  val parameters : t -> Ast.Expression.Parameter.t list

  val decorators : t -> Ast.Expression.t list

  val class_name : t -> string option

  val matches_find : t -> ModelQuery.Find.t -> bool

  val expand_write_to_cache
    :  name_captures:NameCaptures.t ->
    t ->
    ModelQuery.WriteToCache.Substring.t list ->
    string
end

type t = {
  models: Registry.t;
  queries: ModelQuery.t list;
  errors: ModelVerificationError.t list;
}

val empty : t

val join : t -> t -> t
