(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open Interprocedural
open Pyre
module PyrePysaLogic = Analysis.PyrePysaLogic
module PyrePysaApi = Interprocedural.PyrePysaApi
module AccessPath = Analysis.TaintAccessPath

(* ModelParseResult: defines the result of parsing pysa model files (`.pysa`). *)

module CollapseDepth = struct
  type t =
    | Value of int
    | Collapse
    | NoCollapse
  [@@deriving equal]

  let pp formatter = function
    | Value depth -> Format.fprintf formatter "CollapseDepth[%d]" depth
    | Collapse -> Format.fprintf formatter "Collapse"
    | NoCollapse -> Format.fprintf formatter "NoCollapse"


  let show = Format.asprintf "%a" pp
end

module TaintPath = struct
  module Label = struct
    type t =
      | TreeLabel of Abstract.TreeDomain.Label.t
      | ParameterName
    [@@deriving equal]

    let pp formatter = function
      | TreeLabel label when Abstract.TreeDomain.Label.equal label AccessPath.dictionary_keys ->
          Format.fprintf formatter ".keys()"
      | TreeLabel Abstract.TreeDomain.Label.AnyIndex -> Format.fprintf formatter ".all()"
      | ParameterName -> Format.fprintf formatter ".parameter_name()"
      | TreeLabel label -> Abstract.TreeDomain.Label.pp formatter label


    let show = Format.asprintf "%a" pp
  end

  type t =
    | Path of Label.t list
    | AllStaticFields
  [@@deriving equal]

  let pp formatter = function
    | Path path -> List.iter ~f:(Label.pp formatter) path
    | AllStaticFields -> Format.fprintf formatter ".all_static_fields()"


  let show = Format.asprintf "%a" pp

  let has_parameter_name = function
    | Path path -> List.mem path ParameterName ~equal:Label.equal
    | AllStaticFields -> false


  let get_access_path = function
    | Path path ->
        let to_tree_label = function
          | Label.TreeLabel label -> Ok label
          | Label.ParameterName -> Error "parameter_name()"
        in
        path |> List.map ~f:to_tree_label |> Core.Result.all
    | AllStaticFields -> Error "all_static_fields()"
end

module TaintFeatures = struct
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

  let empty =
    {
      breadcrumbs = [];
      via_features = [];
      applies_to = None;
      parameter_path = None;
      return_path = None;
      update_path = None;
      leaf_names = [];
      leaf_name_provided = false;
      trace_length = None;
      collapse_depth = None;
    }


  let is_empty = equal empty

  let join left right =
    let open Core.Result in
    let join_option ~name left right =
      match left, right with
      | Some _, Some _ -> Error (Format.sprintf "%s cannot be specified more than once" name)
      | Some left, None -> Ok (Some left)
      | None, Some right -> Ok (Some right)
      | None, None -> Ok None
    in
    join_option ~name:"AppliesTo" left.applies_to right.applies_to
    >>= fun applies_to ->
    join_option ~name:"ParameterPath" left.parameter_path right.parameter_path
    >>= fun parameter_path ->
    join_option ~name:"ReturnPath" left.return_path right.return_path
    >>= fun return_path ->
    join_option ~name:"UpdatePath" left.update_path right.update_path
    >>= fun update_path ->
    join_option ~name:"trace length" left.trace_length right.trace_length
    >>= fun trace_length ->
    join_option ~name:"collapse depth" left.collapse_depth right.collapse_depth
    >>| fun collapse_depth ->
    {
      breadcrumbs = left.breadcrumbs @ right.breadcrumbs;
      via_features = left.via_features @ right.via_features;
      applies_to;
      parameter_path;
      return_path;
      update_path;
      leaf_names = left.leaf_names @ right.leaf_names;
      leaf_name_provided = left.leaf_name_provided || right.leaf_name_provided;
      trace_length;
      collapse_depth;
    }


  let extend_applies_to features element =
    match features.applies_to with
    | None -> { features with applies_to = Some [element] }
    | Some applies_to -> { features with applies_to = Some (element :: applies_to) }


  let show_as_list
      {
        breadcrumbs;
        via_features;
        applies_to;
        parameter_path;
        return_path;
        update_path;
        leaf_names;
        leaf_name_provided = _;
        trace_length;
        collapse_depth;
      }
    =
    let show_breadcrumb = function
      | Features.Breadcrumb.SimpleVia name -> Format.sprintf "Via[%s]" name
      | breadcrumb -> Format.asprintf "Via[%a]" Features.Breadcrumb.pp breadcrumb
    in
    let features =
      List.map ~f:show_breadcrumb breadcrumbs
      @ List.map ~f:Features.ViaFeature.show via_features
      @ List.map ~f:Features.LeafName.show leaf_names
    in
    let add_option ~name ~pp option features =
      match option with
      | Some value -> features @ [Format.asprintf "%s[%a]" name pp value]
      | None -> features
    in
    let add_collapse_depth features =
      match collapse_depth with
      | Some collapse_depth -> features @ [CollapseDepth.show collapse_depth]
      | None -> features
    in
    features
    |> add_option ~name:"AppliesTo" ~pp:AccessPath.Path.pp applies_to
    |> add_option ~name:"ParameterPath" ~pp:TaintPath.pp parameter_path
    |> add_option ~name:"ReturnPath" ~pp:TaintPath.pp return_path
    |> add_option ~name:"UpdatePath" ~pp:TaintPath.pp update_path
    |> add_option ~name:"TraceLength" ~pp:Int.pp trace_length
    |> add_collapse_depth


  let has_path_with_all_static_fields = function
    | { parameter_path = Some TaintPath.AllStaticFields; _ }
    | { return_path = Some TaintPath.AllStaticFields; _ }
    | { update_path = Some TaintPath.AllStaticFields; _ } ->
        true
    | _ -> false


  let has_path_with_parameter_name = function
    | { parameter_path = Some path; _ } when TaintPath.has_parameter_name path -> true
    | { return_path = Some path; _ } when TaintPath.has_parameter_name path -> true
    | { update_path = Some path; _ } when TaintPath.has_parameter_name path -> true
    | _ -> false
end

module TaintKindsWithFeatures = struct
  type t = {
    kinds: AnnotationParser.KindExpression.t list;
    features: TaintFeatures.t;
  }

  let empty = { kinds = []; features = TaintFeatures.empty }

  let from_kinds kinds = { kinds; features = TaintFeatures.empty }

  let from_kind kind = from_kinds [kind]

  let from_breadcrumbs breadcrumbs =
    { kinds = []; features = { TaintFeatures.empty with breadcrumbs } }


  let from_via_features via_features =
    { kinds = []; features = { TaintFeatures.empty with via_features } }


  let from_via_feature via_feature = from_via_features [via_feature]

  let from_parameter_path path =
    { kinds = []; features = { TaintFeatures.empty with parameter_path = Some path } }


  let from_return_path path =
    { kinds = []; features = { TaintFeatures.empty with return_path = Some path } }


  let from_update_path path =
    { kinds = []; features = { TaintFeatures.empty with update_path = Some path } }


  let from_collapse_depth collapse_depth =
    { kinds = []; features = { TaintFeatures.empty with collapse_depth = Some collapse_depth } }


  let join left right =
    let open Core.Result in
    TaintFeatures.join left.features right.features
    >>| fun features -> { kinds = left.kinds @ right.kinds; features }


  let concat list = List.fold_result ~f:join ~init:empty list
end

module SanitizeAnnotation = struct
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

  let show_as_list = function
    | AllSources -> ["TaintSource"]
    | SpecificSource (SanitizeTransform.Source.Named name) ->
        [Format.sprintf "TaintSource[%s]" name]
    | AllSinks -> ["TaintSink"]
    | SpecificSink (SanitizeTransform.Sink.Named name) -> [Format.sprintf "TaintSink[%s]" name]
    | AllTito -> ["TaintInTaintOut"]
    | SpecificTito { sources; sinks } ->
        List.map
          ~f:(fun (SanitizeTransform.Source.Named name) -> Format.sprintf "TaintSource[%s]" name)
          sources
        @ List.map
            ~f:(fun (SanitizeTransform.Sink.Named name) -> Format.sprintf "TaintSink[%s]" name)
            sinks


  let should_keep ~source_sink_filter annotation =
    let should_keep_source source =
      SourceSinkFilter.should_keep_source source_sink_filter (Sources.from_sanitized_source source)
    in
    let should_keep_sink sink =
      SourceSinkFilter.should_keep_sink source_sink_filter (Sinks.from_sanitized_sink sink)
    in
    match annotation with
    | AllSources -> true
    | SpecificSource source -> should_keep_source source
    | AllSinks -> true
    | SpecificSink sink -> should_keep_sink sink
    | AllTito -> true
    | SpecificTito { sources; sinks } ->
        List.exists ~f:should_keep_source sources || List.exists ~f:should_keep_sink sinks
end

module TaintAnnotation = struct
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
  [@@deriving equal]

  let from_source source = Source { source; features = TaintFeatures.empty }

  let from_sink sink = Sink { sink; features = TaintFeatures.empty }

  let from_tito tito = Tito { tito; features = TaintFeatures.empty }

  let add_cross_repository_anchor ~canonical_name ~canonical_port annotation =
    let leaf_name =
      Features.LeafName.
        { leaf = canonical_name; port = Features.LeafPort.Anchor { port = canonical_port } }
    in
    match annotation with
    | Source { source; features } ->
        Source
          {
            source;
            features =
              {
                features with
                leaf_names = leaf_name :: features.leaf_names;
                leaf_name_provided = true;
              };
          }
    | Sink { sink; features } ->
        Sink
          {
            sink;
            features =
              {
                features with
                leaf_names = leaf_name :: features.leaf_names;
                leaf_name_provided = true;
              };
          }
    | _ -> annotation


  let should_keep ~source_sink_filter = function
    | Sink { sink; _ } -> SourceSinkFilter.should_keep_sink source_sink_filter sink
    | Source { source; _ } -> SourceSinkFilter.should_keep_source source_sink_filter source
    | Tito { tito; _ } -> SourceSinkFilter.should_keep_sink source_sink_filter tito
    | AddFeatureToArgument _ -> true
    | Sanitize annotations ->
        List.exists ~f:(SanitizeAnnotation.should_keep ~source_sink_filter) annotations


  let pp formatter = function
    | Sink { sink; features } ->
        Format.fprintf
          formatter
          "TaintSink[%s]"
          (String.concat ~sep:", " (Sinks.show sink :: TaintFeatures.show_as_list features))
    | Source { source; features } ->
        Format.fprintf
          formatter
          "TaintSource[%s]"
          (String.concat ~sep:", " (Sources.show source :: TaintFeatures.show_as_list features))
    | Tito { tito; features } ->
        Format.fprintf
          formatter
          "TaintInTaintOut[%s]"
          (String.concat ~sep:", " (Sinks.show tito :: TaintFeatures.show_as_list features))
    | AddFeatureToArgument { features } ->
        Format.fprintf
          formatter
          "AddFeatureToArgument[%s]"
          (String.concat ~sep:", " (TaintFeatures.show_as_list features))
    | Sanitize sanitize_annotations ->
        Format.fprintf
          formatter
          "Sanitize[%s]"
          (String.concat
             ~sep:", "
             (sanitize_annotations |> List.map ~f:SanitizeAnnotation.show_as_list |> List.concat))


  let show = Format.asprintf "%a" pp
end

module ModelAnnotation = struct
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
    | AddBreadcrumbsToState of Features.Breadcrumb.t list
  [@@deriving show, equal]
end

module ModelQuery = struct
  module NameConstraint = struct
    type t =
      | Equals of string
      | Matches of Re2.t

    let equal left right =
      match left, right with
      | Equals left, Equals right -> String.equal left right
      | Matches left, Matches right -> Re2.compare left right = 0
      | _, _ -> false


    let pp formatter name_constraint =
      match name_constraint with
      | Equals equals -> Format.fprintf formatter "Equals(%s)" equals
      | Matches regular_expression ->
          Format.fprintf formatter "Matches(%s)" (Re2.to_string regular_expression)


    let show = Format.asprintf "%a" pp
  end

  module AnnotationConstraint = struct
    type t =
      | IsAnnotatedTypeConstraint
      | OriginalAnnotationConstraint of
          NameConstraint.t (* match the user-provided annotation, as seen in the source code *)
      | FullyQualifiedConstraint of NameConstraint.t
        (* match the fully qualified type from the type checker (after parsing, resolving aliases,
           etc.) *)
      | AnnotationClassExtends of {
          class_name: string;
          is_transitive: bool;
          includes_self: bool;
        }
    [@@deriving equal, show]
  end

  module ArgumentsConstraint = struct
    type t =
      | Equals of Ast.Expression.Call.Argument.t list
      | Contains of Ast.Expression.Call.Argument.t list
    [@@deriving show]

    let argument_list_equal left right =
      List.equal
        (fun left right ->
          Int.equal 0 (Ast.Expression.Call.Argument.location_insensitive_compare left right))
        left
        right


    let equal left right =
      match left, right with
      | Equals left, Equals right -> argument_list_equal left right
      | Contains left, Contains right -> argument_list_equal left right
      | _ -> false
  end

  module ParameterConstraint = struct
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

  module DecoratorConstraint = struct
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedCallee of NameConstraint.t
      | ArgumentsConstraint of ArgumentsConstraint.t
      | AnyOf of t list
      | AllOf of t list
      | Not of t
    [@@deriving equal, show]

    let all_of = function
      | [decorator_constraint] -> decorator_constraint
      | decorator_constraints -> AllOf decorator_constraints
  end

  module ClassConstraint = struct
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

  module FormatString = struct
    module IntegerExpression = struct
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

    module Substring = struct
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

  module ReadFromCache = struct
    type t = {
      kind: string;
      name: string;
    }
    [@@deriving equal, show]
  end

  module WriteToCache = struct
    type t = {
      kind: string;
      name: FormatString.t;
    }
    [@@deriving equal, show]
  end

  (* An arbitrary constraint for functions, methods, attributes or globals. *)
  module Constraint = struct
    type t =
      | Constant of bool
      | NameConstraint of NameConstraint.t
      | FullyQualifiedNameConstraint of NameConstraint.t
      | AnnotationConstraint of AnnotationConstraint.t
      | ReturnConstraint of AnnotationConstraint.t
      | AnyParameterConstraint of ParameterConstraint.t
      | AnyOverridenMethod of t
      | ReadFromCache of ReadFromCache.t
      | AnyOf of t list
      | AllOf of t list
      | ClassConstraint of ClassConstraint.t
      | AnyDecoratorConstraint of DecoratorConstraint.t
      | Not of t
    [@@deriving equal, show]

    let rec contains_read_from_cache = function
      | Constant _
      | NameConstraint _
      | FullyQualifiedNameConstraint _
      | AnnotationConstraint _
      | ReturnConstraint _
      | AnyParameterConstraint _
      | ClassConstraint _
      | AnyDecoratorConstraint _ ->
          false
      | ReadFromCache _ -> true
      | AnyOf constraints
      | AllOf constraints ->
          List.exists ~f:contains_read_from_cache constraints
      | Not constraint_
      | AnyOverridenMethod constraint_ ->
          contains_read_from_cache constraint_


    let is_read_from_cache = function
      | ReadFromCache _ -> true
      | _ -> false
  end

  module Find = struct
    type t =
      | Function
      | Method
      | Attribute
      | Global
    [@@deriving equal]

    let from_string = function
      | "functions" -> Some Function
      | "methods" -> Some Method
      | "attributes" -> Some Attribute
      | "globals" -> Some Global
      | _ -> None


    let pp formatter = function
      | Attribute -> Format.fprintf formatter "attributes"
      | Method -> Format.fprintf formatter "methods"
      | Function -> Format.fprintf formatter "functions"
      | Global -> Format.fprintf formatter "globals"


    let show = Format.asprintf "%a" pp

    let is_callable = function
      | Function -> true
      | Method -> true
      | _ -> false


    let is_global = equal Global

    let is_attribute = equal Attribute

    let is_class_member = function
      | Method -> true
      | Attribute -> true
      | _ -> false
  end

  module QueryTaintAnnotation = struct
    type t =
      | TaintAnnotation of TaintAnnotation.t
      | CrossRepositoryTaintAnchor of {
          annotation: TaintAnnotation.t;
          canonical_name: FormatString.t;
          canonical_port: FormatString.t;
        }
    [@@deriving show, equal]

    let should_keep ~source_sink_filter = function
      | TaintAnnotation taint -> TaintAnnotation.should_keep ~source_sink_filter taint
      | CrossRepositoryTaintAnchor { annotation; _ } ->
          TaintAnnotation.should_keep ~source_sink_filter annotation
  end

  module ExpectedModel = struct
    type t = {
      model: Model.t;
      target: Target.t;
      model_source: string;
    }

    let pp formatter model_with_target_and_string =
      Format.fprintf
        formatter
        "Model:\n[\n%s\n]\nTarget: \"%s\"\nModel source: \"%s\""
        (Model.show model_with_target_and_string.model)
        (Target.show_pretty model_with_target_and_string.target)
        model_with_target_and_string.model_source


    let show = Format.asprintf "%a" pp

    let equal left right =
      Model.equal left.model right.model
      && Target.equal left.target right.target
      && String.equal left.model_source right.model_source


    let less_or_equal left right =
      Model.less_or_equal ~left:left.model ~right:right.model
      && Target.equal left.target right.target
      && String.equal left.model_source right.model_source
  end

  module Model = struct
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

    let is_write_to_cache = function
      | WriteToCache _ -> true
      | _ -> false


    let should_keep ~source_sink_filter = function
      | AllParameters { taint; _ }
      | NamedParameter { taint; _ }
      | PositionalParameter { taint; _ }
      | Parameter { taint; _ }
      | Return taint
      | CapturedVariables { taint; _ }
      | Attribute taint
      | Global taint ->
          List.exists ~f:(QueryTaintAnnotation.should_keep ~source_sink_filter) taint
      | Modes _
      | WriteToCache _ ->
          true
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

  let unique_identifier = function
    | { name; path = None; _ } -> name
    | { name; path = Some path; _ } -> Format.sprintf "%s/%s" (PyrePath.get_suffix_path path) name


  let extract_extends_from_model_queries model_queries =
    let rec process_class_constraint result = function
      | ClassConstraint.Extends { class_name; _ } -> class_name :: result
      | ClassConstraint.AnyOf constraints
      | ClassConstraint.AllOf constraints ->
          List.fold ~f:process_class_constraint ~init:result constraints
      | ClassConstraint.Not constraint_ -> process_class_constraint result constraint_
      | _ -> result
    in
    let process_annotation_constraint result = function
      | AnnotationConstraint.AnnotationClassExtends { class_name; _ } -> class_name :: result
      | _ -> result
    in
    let rec process_parameter_constraint result = function
      | ParameterConstraint.AnnotationConstraint annotation_constraint ->
          process_annotation_constraint result annotation_constraint
      | ParameterConstraint.AnyOf constraints
      | ParameterConstraint.AllOf constraints ->
          List.fold ~f:process_parameter_constraint ~init:result constraints
      | ParameterConstraint.Not constraint_ -> process_parameter_constraint result constraint_
      | _ -> result
    in
    let rec process_constraint result = function
      | Constraint.ClassConstraint class_constraint ->
          process_class_constraint result class_constraint
      | Constraint.AnnotationConstraint annotation_constraint ->
          process_annotation_constraint result annotation_constraint
      | Constraint.AnyParameterConstraint parameter_constraint ->
          process_parameter_constraint result parameter_constraint
      | Constraint.AnyOf constraints
      | Constraint.AllOf constraints ->
          List.fold ~f:process_constraint ~init:result constraints
      | Constraint.Not constraint_ -> process_constraint result constraint_
      | _ -> result
    in
    List.fold
      ~f:(fun result model_query -> List.fold ~f:process_constraint ~init:result model_query.where)
      ~init:[]
      model_queries


  let should_keep ~source_sink_filter { models; _ } =
    List.exists ~f:(Model.should_keep ~source_sink_filter) models
end

(* Store all regular expression captures in name constraints for WriteToCache queries. *)
module NameCaptures = struct
  type t = Re2.Match.t list ref

  let create () = ref []

  let add results name_match = results := name_match :: !results

  let get results identifier =
    List.find_map !results ~f:(fun name_match -> Re2.Match.get ~sub:(`Name identifier) name_match)
end

module CallableDecorator = struct
  type t = {
    statement: Statement.Decorator.t;
    callees: Reference.t list Lazy.t option;
  }

  let create ~pyre_api ~callables_to_definitions_map ~qualifier ~target statement =
    let get_callees statement =
      let ({ Node.value = expression; _ } as decorator_expression) =
        Statement.Decorator.to_expression statement
      in
      let callee =
        match expression with
        | Expression.Expression.Call { callee; _ } ->
            (* Decorator factory, such as `@foo(1)` *) callee
        | Expression.Expression.Name _ ->
            (* Regular decorator, such as `@foo` *) decorator_expression
        | _ -> decorator_expression
      in
      match pyre_api with
      | PyrePysaApi.ReadOnly.Pyre1 pyre_api ->
          let return_type =
            (* Since this won't be used and resolving the return type could be expensive, let's pass
               a random type. *)
            lazy Type.Any
          in
          let { Interprocedural.CallGraph.CallCallees.call_targets; new_targets; init_targets; _ } =
            Interprocedural.CallGraphBuilder.resolve_callees_from_type_external
              ~pyre_in_context:
                (PyrePysaApi.InContext.Pyre1
                   (Analysis.PyrePysaEnvironment.InContext.create_at_function_scope
                      pyre_api
                      ~module_qualifier:qualifier
                      ~define_name:(Target.define_name_exn target)))
              ~callables_to_definitions_map
              ~override_graph:None
              ~return_type
              callee
          in
          let call_targets =
            call_targets |> List.rev_append new_targets |> List.rev_append init_targets
          in
          let call_target_to_fully_qualified_name call_target =
            call_target
            |> CallGraph.CallTarget.target
            |> Target.get_regular
            |> Target.Regular.override_to_method
            |> Target.Regular.define_name_exn
          in
          List.map ~f:call_target_to_fully_qualified_name call_targets
      | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
          Interprocedural.PyreflyApi.ReadOnly.get_callable_decorator_callees
            pyrefly_api
            (Target.define_name_exn target)
            (Node.location callee)
          |> Option.value ~default:[]
          |> List.map ~f:Interprocedural.PyreflyApi.target_symbolic_name
    in
    let callees = Some (lazy (get_callees statement)) in
    { statement; callees }


  let create_without_callees statement = { statement; callees = None }

  let statement { statement; _ } = statement

  let callees { callees; _ } = Option.map ~f:Lazy.force callees
end

module TypeAnnotation : sig
  type t

  module ExplicitAnnotation : sig
    type t =
      | Unsupported
      | NotFound
      | Found of string
  end

  val from_pyre1 : pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t -> Expression.t option -> t

  val create
    :  inferred_type:PyrePysaApi.PysaType.t option ->
    explicit_annotation:ExplicitAnnotation.t ->
    t

  val from_inferred_type : PyrePysaApi.PysaType.t option -> t

  val is_annotated : t -> bool

  val inferred_type : t -> PyrePysaApi.PysaType.t option

  val explicit_annotation : t -> ExplicitAnnotation.t

  (* Show the original annotation, as written by the user. *)
  val show_explicit_annotation : t -> string option

  (* Show the fully qualified type annotation from the type checker. *)
  val show_fully_qualified_annotation : t -> string
end = struct
  module ExplicitAnnotation = struct
    type t =
      | Unsupported
      | NotFound
      | Found of string
  end

  type t = {
    explicit_annotation: ExplicitAnnotation.t Lazy.t;
    inferred_type: PyrePysaApi.PysaType.t option Lazy.t;
  }

  let from_pyre1 ~pyre_api = function
    | Some expression ->
        {
          explicit_annotation = lazy (ExplicitAnnotation.Found (Expression.show expression));
          inferred_type =
            lazy
              (Analysis.PyrePysaEnvironment.ReadOnly.parse_annotation pyre_api expression
              |> PyrePysaApi.PysaType.from_pyre1_type
              |> Option.some);
        }
    | None -> { explicit_annotation = lazy ExplicitAnnotation.NotFound; inferred_type = lazy None }


  let create ~inferred_type ~explicit_annotation =
    { explicit_annotation = lazy explicit_annotation; inferred_type = lazy inferred_type }


  let from_inferred_type inferred_type =
    {
      explicit_annotation = lazy ExplicitAnnotation.Unsupported;
      inferred_type = lazy inferred_type;
    }


  let is_annotated { explicit_annotation; _ } =
    match Lazy.force explicit_annotation with
    | ExplicitAnnotation.Unsupported -> failwith "is_annotated is not supported in this context"
    | ExplicitAnnotation.Found annotation ->
        (String.is_prefix ~prefix:"typing.Annotated[" annotation
        || String.is_prefix ~prefix:"Annotated[" annotation)
        && String.is_suffix ~suffix:"]" annotation
    | ExplicitAnnotation.NotFound -> false


  let inferred_type { inferred_type; _ } = Lazy.force inferred_type

  let explicit_annotation { explicit_annotation; _ } = Lazy.force explicit_annotation

  (* Show the original annotation, as written by the user. *)
  let show_explicit_annotation { explicit_annotation; _ } =
    match Lazy.force explicit_annotation with
    | ExplicitAnnotation.Unsupported ->
        failwith "show_explicit_annotation is not supported in this context"
    | ExplicitAnnotation.Found annotation -> Some annotation
    | ExplicitAnnotation.NotFound -> None


  (* Show the parsed annotation from pyre *)
  let show_fully_qualified_annotation { inferred_type; _ } =
    match Lazy.force inferred_type with
    | Some inferred_type -> PyrePysaApi.PysaType.show_fully_qualified inferred_type
    | None -> "typing.Any"
end

module Modelable = struct
  (* Use lazy values so we only query information when required. *)
  type t =
    | Callable of {
        target: Target.t;
        (* The syntactic definition of the function, including the AST for each parameters. *)
        define_signature: CallablesSharedMemory.CallableSignature.t Lazy.t;
        (* The semantic (undecorated) signature(s) of the function. *)
        undecorated_signatures: PyrePysaApi.ModelQueries.FunctionSignature.t list Lazy.t;
        decorators: CallableDecorator.t list Lazy.t;
        captures: Analysis.TaintAccessPath.CapturedVariable.t list Lazy.t;
      }
    | Attribute of {
        target_name: Reference.t;
        type_annotation: TypeAnnotation.t Lazy.t;
      }
    | Global of {
        target_name: Reference.t;
        type_annotation: TypeAnnotation.t Lazy.t;
      }

  let create_callable ~pyre_api ~callables_to_definitions_map target =
    let define_signature =
      lazy
        (match CallablesSharedMemory.ReadOnly.get_signature callables_to_definitions_map target with
        | None ->
            Format.asprintf
              "unknown target `%a` in `Modelable.create_callable`"
              Target.pp_external
              target
            |> failwith
        | Some signature -> signature)
    in
    let undecorated_signatures =
      lazy
        (match pyre_api with
        | PyrePysaApi.ReadOnly.Pyre1 pyre_api ->
            Lazy.force define_signature
            |> fun { CallablesSharedMemory.CallableSignature.parameters; return_annotation; _ } ->
            Analysis.PyrePysaEnvironment.ModelQueries.FunctionSignature.from_pyre1_ast
              ~pyre_api
              ~parameters:(PyrePysaApi.AstResult.value_exn ~message:"unreachable" parameters)
              ~return_annotation:
                (PyrePysaApi.AstResult.value_exn ~message:"unreachable" return_annotation)
            |> fun signature -> [signature]
        | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
            Interprocedural.PyreflyApi.ReadOnly.get_undecorated_signatures
              pyrefly_api
              (Target.define_name_exn target))
    in
    let decorators =
      lazy
        (define_signature
        |> Lazy.force
        |> (fun { CallablesSharedMemory.CallableSignature.define_name; decorators; qualifier; _ } ->
             PyrePysaApi.AstResult.to_option decorators
             >>| (fun decorators ->
                   PyrePysaLogic.DecoratorPreprocessing
                   .original_decorators_from_preprocessed_signature
                     ~define_name
                     ~decorators)
             >>| List.filter_map ~f:Statement.Decorator.from_expression
             >>| List.map
                   ~f:
                     (CallableDecorator.create
                        ~pyre_api
                        ~callables_to_definitions_map
                        ~qualifier
                        ~target))
        |> Option.value ~default:[])
    in
    let captures =
      lazy
        (match pyre_api with
        | PyrePysaApi.ReadOnly.Pyre1 _ ->
            Lazy.force define_signature
            |> fun { CallablesSharedMemory.CallableSignature.captures; _ } -> captures
        | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
            PyreflyApi.ReadOnly.get_callable_captures pyrefly_api (Target.define_name_exn target))
    in
    Callable { target; define_signature; undecorated_signatures; decorators; captures }


  let create_attribute ~pyre_api target =
    let target_name = Target.object_name target in
    let type_annotation =
      lazy
        ((* TODO(T225700656): Add API to get class name from attribute name *)
         let class_name =
           Reference.prefix target_name >>| Reference.show |> Option.value ~default:""
         in
         let attribute = Reference.last target_name in
         match pyre_api with
         | PyrePysaApi.ReadOnly.Pyre1 pyre1_api ->
             Analysis.PyrePysaEnvironment.ReadOnly.get_class_attribute_annotation
               pyre1_api
               ~include_generated_attributes:false
               ~class_name
               ~attribute
             |> TypeAnnotation.from_pyre1 ~pyre_api:pyre1_api
         | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
             let inferred_type =
               Interprocedural.PyreflyApi.ReadOnly.get_class_attribute_inferred_type
                 pyrefly_api
                 ~class_name
                 ~attribute
               |> Option.some
             in
             let explicit_annotation =
               Interprocedural.PyreflyApi.ReadOnly.get_class_attribute_explicit_annotation
                 pyrefly_api
                 ~class_name
                 ~attribute
               |> function
               | Some annotation -> TypeAnnotation.ExplicitAnnotation.Found annotation
               | None -> TypeAnnotation.ExplicitAnnotation.NotFound
             in
             TypeAnnotation.create ~inferred_type ~explicit_annotation)
    in
    Attribute { target_name; type_annotation }


  let create_global ~pyre_api target =
    let target_name = Target.object_name target in
    let type_annotation =
      lazy
        (match pyre_api with
        | PyrePysaApi.ReadOnly.Pyre1 pyre1_api ->
            Analysis.PyrePysaEnvironment.ReadOnly.get_global_annotation pyre1_api target_name
            |> TypeAnnotation.from_pyre1 ~pyre_api:pyre1_api
        | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
            let qualifier = Option.value_exn (Reference.prefix target_name) in
            let name = Reference.last target_name in
            let inferred_type =
              Interprocedural.PyreflyApi.ReadOnly.get_global_inferred_type
                pyrefly_api
                ~qualifier
                ~name
            in
            TypeAnnotation.create
              ~inferred_type
              ~explicit_annotation:TypeAnnotation.ExplicitAnnotation.Unsupported)
    in
    Global { target_name; type_annotation }


  let target = function
    | Callable { target; _ } -> target
    | Attribute { target_name; _ }
    | Global { target_name; _ } ->
        Target.create_object target_name


  let target_name = function
    | Callable { target; _ } -> Target.define_name_exn target
    | Attribute { target_name; _ }
    | Global { target_name; _ } ->
        target_name


  let type_annotation = function
    | Callable _ -> failwith "unexpected use of type_annotation on a callable"
    | Attribute { type_annotation; _ }
    | Global { type_annotation; _ } ->
        Lazy.force type_annotation


  let undecorated_signatures = function
    | Callable { undecorated_signatures; _ } -> Lazy.force undecorated_signatures
    | Attribute _
    | Global _ ->
        failwith "unexpected use of undecorated_signatures on an attribute or global"


  let return_annotations = function
    | Callable { undecorated_signatures; _ } ->
        undecorated_signatures
        |> Lazy.force
        |> List.map ~f:(fun { PyrePysaApi.ModelQueries.FunctionSignature.return_annotation; _ } ->
               return_annotation)
    | Attribute _
    | Global _ ->
        failwith "unexpected use of return_annotation on an attribute or global"


  let parameters_of_signatures = function
    | Callable { undecorated_signatures; _ } ->
        undecorated_signatures
        |> Lazy.force
        |> List.map ~f:(fun { PyrePysaApi.ModelQueries.FunctionSignature.parameters; _ } ->
               parameters)
        |> List.filter_map ~f:(function
               | PyrePysaApi.ModelQueries.FunctionParameters.List parameters -> Some parameters
               | _ -> None)
        |> List.concat
    | Attribute _
    | Global _ ->
        failwith "unexpected use of any_parameter on an attribute or global"


  let captures = function
    | Callable { captures; _ } -> Lazy.force captures
    | Attribute _
    | Global _ ->
        failwith "unexpected use of captures on an attribute or global"


  let decorator_expressions_after_inlining = function
    | Callable { define_signature; _ } ->
        Lazy.force define_signature
        |> (fun { CallablesSharedMemory.CallableSignature.decorators; _ } -> decorators)
        |> PyrePysaApi.AstResult.to_option
        |> Option.value ~default:[]
    | Attribute _
    | Global _ ->
        failwith "unexpected use of decorator_expression on an attribute or global"


  let resolved_original_decorators = function
    | Callable { decorators; _ } -> Lazy.force decorators
    | Attribute _
    | Global _ ->
        failwith "unexpected use of resolved_decorators on an attribute or global"


  let is_instance_method = function
    | Callable { define_signature; _ } ->
        Lazy.force define_signature
        |> (fun { CallablesSharedMemory.CallableSignature.method_kind; _ } -> method_kind)
        >>| (function
              | Target.MethodKind.Instance -> true
              | _ -> false)
        |> Option.value ~default:false
    | Attribute _
    | Global _ ->
        false


  let class_name = function
    | Callable { target; _ } -> Target.class_name target
    | Attribute { target_name; _ } ->
        (* TODO(T225700656): Add API to get class name from attribute name *)
        Reference.prefix target_name >>| Reference.show
    | Global _ -> failwith "unexpected use of a class constraint on a global"


  let matches_find modelable find =
    match find, modelable with
    | ModelQuery.Find.Function, Callable { target; _ } when Target.is_function target -> true
    | ModelQuery.Find.Method, Callable { target; _ } when Target.is_method target -> true
    | ModelQuery.Find.Attribute, Attribute _
    | ModelQuery.Find.Global, Global _ ->
        true
    | _ -> false


  let expand_format_string ~name_captures ~parameter modelable name =
    let open Core.Result in
    let expand_function_name () =
      let error = Error "`function_name` can only be used on functions" in
      match modelable with
      | Callable { target; _ } -> (
          match Target.function_name target with
          | Some name -> Reference.create name |> Reference.last |> Result.return
          | None -> error)
      | _ -> error
    in
    let expand_method_name () =
      let error = Error "`method_name` can only be used on methods" in
      match modelable with
      | Callable { target; _ } -> (
          match Target.method_name target with
          | Some method_name -> Ok method_name
          | _ -> error)
      | _ -> error
    in
    let expand_class_name () =
      let error = Error "`class_name` can only be used on methods" in
      match modelable with
      | Callable { target; _ } -> (
          match Target.class_name target with
          | Some class_name -> Reference.create class_name |> Reference.last |> Result.return
          | None -> error)
      | _ -> error
    in
    let expand_capture identifier =
      match NameCaptures.get name_captures identifier with
      | Some value -> Ok value
      | None ->
          let () = Log.warning "No match for capture `%s` in WriteToCache query" identifier in
          Ok ""
    in
    let expand_parameter_name () =
      match parameter with
      | None
      | Some AccessPath.Root.LocalResult
      | Some (AccessPath.Root.Variable _)
      | Some (AccessPath.Root.CapturedVariable _) ->
          Error "`parameter_name` can only be used on parameters"
      | Some (AccessPath.Root.PositionalParameter { name; _ }) -> Ok name
      | Some (AccessPath.Root.NamedParameter { name }) -> Ok name
      | Some (AccessPath.Root.StarParameter _) -> Ok "*args"
      | Some (AccessPath.Root.StarStarParameter _) -> Ok "**kwargs"
    in
    let expand_parameter_position () =
      match parameter with
      | None
      | Some AccessPath.Root.LocalResult
      | Some (AccessPath.Root.Variable _)
      | Some (AccessPath.Root.CapturedVariable _) ->
          Error "`parameter_position` can only be used on parameters"
      | Some (AccessPath.Root.PositionalParameter { position; _ }) -> Ok position
      | Some (AccessPath.Root.StarParameter { position }) -> Ok position
      | Some (AccessPath.Root.NamedParameter _)
      | Some (AccessPath.Root.StarStarParameter _) ->
          (* These don't have a position, let's use -1. Do not throw an error since this can easily
             be triggered on code changes. To prevent issues, the user should use a model query
             constraint to match on positional parameters only. *)
          Ok (-1)
    in
    let rec expand_integer_expression = function
      | ModelQuery.FormatString.IntegerExpression.Constant value -> Ok value
      | ParameterPosition -> expand_parameter_position ()
      | Add { left; right } ->
          expand_integer_expression left
          >>= fun left -> expand_integer_expression right >>| fun right -> left + right
      | Sub { left; right } ->
          expand_integer_expression left
          >>= fun left -> expand_integer_expression right >>| fun right -> left - right
      | Mul { left; right } ->
          expand_integer_expression left
          >>= fun left -> expand_integer_expression right >>| fun right -> left * right
    in
    let expand_substring = function
      | ModelQuery.FormatString.Substring.Literal value -> Ok value
      | FunctionName -> expand_function_name ()
      | MethodName -> expand_method_name ()
      | ClassName -> expand_class_name ()
      | Capture identifier -> expand_capture identifier
      | ParameterName -> expand_parameter_name ()
      | Integer expression -> expression |> expand_integer_expression >>| string_of_int
    in
    name |> List.map ~f:expand_substring |> Result.all |> Result.map ~f:(String.concat ~sep:"")
end

type t = {
  models: Registry.t;
  queries: ModelQuery.t list;
  errors: ModelVerificationError.t list;
}

let empty = { models = Registry.empty; queries = []; errors = [] }

let join
    { models = models_left; queries = queries_left; errors = errors_left }
    { models = models_right; queries = queries_right; errors = errors_right }
  =
  {
    models = Registry.merge ~join:Model.join_user_models models_left models_right;
    queries = List.rev_append queries_right queries_left;
    errors = List.rev_append errors_right errors_left;
  }
