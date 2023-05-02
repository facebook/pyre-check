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

(* ModelParseResult: defines the result of parsing pysa model files (`.pysa`). *)

(* Represents a source or sink kind (e.g, UserControlled) *)
module Kind = struct
  type t = {
    name: string;
    subkind: string option;
  }
  [@@deriving equal]

  let from_name name = { name; subkind = None }
end

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

module TaintFeatures = struct
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
    let add_path_option ~name path features =
      add_option ~name ~pp:Abstract.TreeDomain.Label.pp_path path features
    in
    let add_collapse_depth features =
      match collapse_depth with
      | Some collapse_depth -> features @ [CollapseDepth.show collapse_depth]
      | None -> features
    in
    features
    |> add_path_option ~name:"AppliesTo" applies_to
    |> add_path_option ~name:"ParameterPath" parameter_path
    |> add_path_option ~name:"ReturnPath" return_path
    |> add_path_option ~name:"UpdatePath" update_path
    |> add_option ~name:"TraceLength" ~pp:Int.pp trace_length
    |> add_collapse_depth
end

module TaintKindsWithFeatures = struct
  type t = {
    kinds: Kind.t list;
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
    | ParameterAnnotation of AccessPath.Root.t * TaintAnnotation.t
    | ReturnAnnotation of TaintAnnotation.t
    | ModeAnnotation of Model.ModeSet.t
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
      | NameConstraint of NameConstraint.t
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
      | AnyOf of t list
      | AllOf of t list
      | Not of t
    [@@deriving equal, show]
  end

  module DecoratorConstraint = struct
    type t =
      | NameConstraint of NameConstraint.t
      | FullyQualifiedNameConstraint of NameConstraint.t
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

  module ReadFromCache = struct
    type t = {
      kind: string;
      name: string;
    }
    [@@deriving equal, show]
  end

  module WriteToCache = struct
    module Substring = struct
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
  module Constraint = struct
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

    let rec contains_read_from_cache = function
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
      | Not constraint_ -> contains_read_from_cache constraint_


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

    let less_or_equal left right =
      Model.less_or_equal ~left:left.model ~right:right.model
      && Target.equal left.target right.target
      && String.equal left.model_source right.model_source


    let equal left right = less_or_equal left right && less_or_equal right left
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
      | Attribute of QueryTaintAnnotation.t list
      | Global of QueryTaintAnnotation.t list
      | Modes of Model.ModeSet.t
      | WriteToCache of WriteToCache.t
    [@@deriving show, equal]

    let is_write_to_cache = function
      | WriteToCache _ -> true
      | _ -> false
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
end

(* Store all regular expression captures in name constraints for WriteToCache queries. *)
module NameCaptures = struct
  type t = Re2.Match.t list ref

  let create () = ref []

  let add results name_match = results := name_match :: !results

  let get results identifier =
    List.find_map !results ~f:(fun name_match -> Re2.Match.get ~sub:(`Name identifier) name_match)
end

module Modelable = struct
  (* Use lazy values so we only query information when required. *)
  type t =
    | Callable of {
        target: Target.t;
        signature: Statement.Define.Signature.t Lazy.t;
      }
    | Attribute of {
        name: Reference.t;
        type_annotation: Expression.t option Lazy.t;
      }
    | Global of {
        name: Reference.t;
        type_annotation: Expression.t option Lazy.t;
      }

  let target = function
    | Callable { target; _ } -> target
    | Attribute { name; _ }
    | Global { name; _ } ->
        Target.create_object name


  let name = function
    | Callable { target; _ } -> Target.define_name target
    | Attribute { name; _ }
    | Global { name; _ } ->
        name


  let type_annotation = function
    | Callable _ -> failwith "unexpected use of type_annotation on a callable"
    | Attribute { type_annotation; _ }
    | Global { type_annotation; _ } ->
        Lazy.force type_annotation


  let return_annotation = function
    | Callable { signature; _ } ->
        let { Statement.Define.Signature.return_annotation; _ } = Lazy.force signature in
        return_annotation
    | Attribute _
    | Global _ ->
        failwith "unexpected use of return_annotation on an attribute or global"


  let parameters = function
    | Callable { signature; _ } ->
        let { Statement.Define.Signature.parameters; _ } = Lazy.force signature in
        parameters
    | Attribute _
    | Global _ ->
        failwith "unexpected use of any_parameter on an attribute or global"


  let decorators = function
    | Callable { signature; _ } ->
        signature
        |> Lazy.force
        |> Analysis.DecoratorPreprocessing.original_decorators_from_preprocessed_signature
    | Attribute _
    | Global _ ->
        failwith "unexpected use of Decorator on an attribute or global"


  let class_name = function
    | Callable { target; _ } -> Target.class_name target
    | Attribute { name; _ } -> Reference.prefix name >>| Reference.show
    | Global _ -> failwith "unexpected use of a class constraint on a global"


  let matches_find modelable find =
    match find, modelable with
    | ModelQuery.Find.Function, Callable { target = Target.Function _; _ }
    | ModelQuery.Find.Method, Callable { target = Target.Method _; _ }
    | ModelQuery.Find.Attribute, Attribute _
    | ModelQuery.Find.Global, Global _ ->
        true
    | _ -> false


  let expand_write_to_cache ~name_captures modelable name =
    let expand_substring modelable substring =
      match substring, modelable with
      | ModelQuery.WriteToCache.Substring.Literal value, _ -> value
      | FunctionName, Callable { target = Target.Function { name; _ }; _ } ->
          Reference.create name |> Reference.last
      | MethodName, Callable { target = Target.Method { method_name; _ }; _ } -> method_name
      | ClassName, Callable { target = Target.Method { class_name; _ }; _ } ->
          Reference.create class_name |> Reference.last
      | Capture identifier, _ -> (
          match NameCaptures.get name_captures identifier with
          | Some value -> value
          | None ->
              let () = Log.warning "No match for capture `%s` in WriteToCache query" identifier in
              "")
      | _ -> failwith "unreachable"
    in
    name |> List.map ~f:(expand_substring modelable) |> String.concat ~sep:""
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
    models = Registry.merge_skewed ~join:Model.join_user_models models_left models_right;
    queries = List.rev_append queries_right queries_left;
    errors = List.rev_append errors_right errors_left;
  }
