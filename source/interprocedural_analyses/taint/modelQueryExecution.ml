(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ModelQueryExecution: implements the logic that generates models from queries.
 *
 * A model query defines a taint to attach to a set of targets. Targets are defined
 * by a set of constraints (e.g, "find all functions starting with foo").
 *)

open Core
open Data_structures
open Pyre
open Ast
open Interprocedural
open ModelParseResult
module PyrePysaApi = Interprocedural.PyrePysaApi
module FunctionParameter = PyrePysaApi.ModelQueries.FunctionParameter

(* Represents results from model queries, stored in the ocaml heap. *)
module ModelQueryResultMap = struct
  type t = Registry.t String.Map.t

  let empty = (String.Map.empty : t)

  let add_registry model_query_results ~model_join ~query registry =
    if not (Registry.is_empty registry) then
      Map.update model_query_results (ModelQuery.unique_identifier query) ~f:(function
          | None -> registry
          | Some existing -> Registry.merge ~join:model_join existing registry)
    else
      model_query_results


  let add_model model_query_results ~model_join ~query ~target ~model =
    Map.update model_query_results (ModelQuery.unique_identifier query) ~f:(function
        | None -> Registry.singleton ~target ~model
        | Some existing -> Registry.add ~join:model_join existing ~target ~model)


  let get_targets model_query_results =
    model_query_results
    |> Map.fold ~init:Target.Set.empty ~f:(fun ~key:_ ~data:registry targets ->
           Target.Set.union targets (registry |> Registry.targets |> Target.Set.of_list))


  let number_models model_query_results = model_query_results |> get_targets |> Target.Set.cardinal

  let merge_all_registries ~model_join registries =
    Algorithms.fold_balanced registries ~init:Registry.empty ~f:(Registry.merge ~join:model_join)


  let get_registry ~model_join model_query_map =
    merge_all_registries ~model_join (Map.data model_query_map)
end

(* Represents results from a model query, but only what is necessary to check `expected_models` and
   `unexpected_models` clauses. *)
module ModelQueryExpectedResults = struct
  type t = {
    (* Only store models for targets present in `expected_models` and `unexpected_models`. *)
    models_for_expected: Registry.t;
    (* Number of models generated from the query (regardless of `expected_models`). *)
    number_models: int;
  }

  let empty = { models_for_expected = Registry.empty; number_models = 0 }

  let number_models { number_models; _ } = number_models

  let merge_disjoint
      { models_for_expected = left_models; number_models = left_number_models }
      { models_for_expected = right_models; number_models = right_number_models }
    =
    {
      models_for_expected =
        Registry.merge
          ~join:(fun _ _ -> failwith "merge_disjoint: non-disjoint")
          left_models
          right_models;
      number_models = left_number_models + right_number_models;
    }


  let add_model_for_new_target
      { models_for_expected; number_models }
      ~target
      ~query:
        {
          ModelQuery.expected_models = query_expected_models;
          unexpected_models = query_unexpected_models;
          _;
        }
      ~model
    =
    let is_expected =
      List.exists
        query_expected_models
        ~f:(fun { ModelQuery.ExpectedModel.target = expected_target; _ } ->
          Target.equal target expected_target)
      || List.exists
           query_unexpected_models
           ~f:(fun { ModelQuery.ExpectedModel.target = expected_target; _ } ->
             Target.equal target expected_target)
    in
    let models_for_expected =
      if is_expected then
        Registry.add ~join:(fun _ _ -> failwith "non-disjoint") models_for_expected ~target ~model
      else
        models_for_expected
    in
    let number_models = number_models + 1 in
    { models_for_expected; number_models }
end

(* Same as `ModelQueryExpectedResults`, but for multiple model queries *)
module ModelQueriesExpectedResults = struct
  type t = ModelQueryExpectedResults.t String.Map.t

  let empty = String.Map.empty

  let get model_query_results query =
    Map.find model_query_results (ModelQuery.unique_identifier query)


  let add_model_for_new_target model_query_results ~target ~query ~model =
    Map.update model_query_results (ModelQuery.unique_identifier query) ~f:(fun existing ->
        ModelQueryExpectedResults.add_model_for_new_target
          (Option.value existing ~default:ModelQueryExpectedResults.empty)
          ~target
          ~query
          ~model)


  let merge_disjoint left right =
    Map.merge_skewed left right ~combine:(fun ~key:_ left_models right_models ->
        ModelQueryExpectedResults.merge_disjoint left_models right_models)


  let add_models_for_new_queries model_query_results ~queries:_ other =
    (* For now, only merge model counts. In the future, we should preserve models for queries with
       `expected_models` clauses. *)
    let other =
      Map.map
        ~f:(fun registry ->
          {
            ModelQueryExpectedResults.models_for_expected = Registry.empty;
            number_models = Registry.size registry;
          })
        other
    in
    Map.merge_skewed ~combine:(fun ~key:_ _ _ -> failwith "non-disjoint") model_query_results other


  let check_expected_and_unexpected_model_errors model_query_results ~queries =
    let find_expected_and_unexpected_model_errors
        ~expect
        ~actual_models
        ~name
        ~path
        ~location
        ~models
      =
      let registry_contains_model registry ~target ~model =
        (* TODO T127682824: Deal with the case of joined models *)
        match Registry.get registry target with
        | Some actual_model -> Model.less_or_equal ~left:model ~right:actual_model
        | None -> false
      in
      let expected_and_unexpected_models =
        List.filter_map models ~f:(fun { ModelQuery.ExpectedModel.model; target; model_source } ->
            let unexpected =
              if expect then
                not (registry_contains_model actual_models ~target ~model)
              else
                registry_contains_model actual_models ~target ~model
            in
            if unexpected then Some model_source else None)
      in
      match expected_and_unexpected_models with
      | [] -> []
      | models ->
          let kind =
            if expect then
              ModelVerificationError.ExpectedModelsAreMissing { model_query_name = name; models }
            else
              ModelVerificationError.UnexpectedModelsArePresent { model_query_name = name; models }
          in
          [{ ModelVerificationError.kind; location; path }]
    in
    let find_expected_model_errors ~actual_models ~name ~path ~location ~expected_models =
      find_expected_and_unexpected_model_errors
        ~expect:true
        ~actual_models
        ~name
        ~path
        ~location
        ~models:expected_models
    in
    let find_unexpected_model_errors ~actual_models ~name ~path ~location ~unexpected_models =
      find_expected_and_unexpected_model_errors
        ~expect:false
        ~actual_models
        ~name
        ~path
        ~location
        ~models:unexpected_models
    in
    let expected_and_unexpected_model_errors =
      queries
      |> List.map
           ~f:(fun
                ({ ModelQuery.name; path; location; expected_models; unexpected_models; _ } as
                model_query)
              ->
             let { ModelQueryExpectedResults.models_for_expected = actual_models; _ } =
               Option.value
                 (get model_query_results model_query)
                 ~default:ModelQueryExpectedResults.empty
             in
             let expected_model_errors =
               match expected_models with
               | [] -> []
               | _ ->
                   find_expected_model_errors ~actual_models ~name ~path ~location ~expected_models
             in
             let unexpected_model_errors =
               match unexpected_models with
               | [] -> []
               | _ ->
                   find_unexpected_model_errors
                     ~actual_models
                     ~name
                     ~path
                     ~location
                     ~unexpected_models
             in
             List.append expected_model_errors unexpected_model_errors)
      |> List.concat
    in
    expected_and_unexpected_model_errors


  let errors_for_queries_without_output model_query_results ~queries =
    let module LoggingGroup = struct
      type t = {
        models_count: int;
        (* Location and path of the first query in the group. *)
        location: Location.t;
        path: PyrePath.t option;
      }

      let add ({ models_count; _ } as model_group) count =
        { model_group with models_count = models_count + count }
    end
    in
    let count_models
        (logging_group_map, errors)
        ({ ModelQuery.name; logging_group_name; location; path; _ } as query)
      =
      let models_count =
        get model_query_results query
        |> Option.value ~default:ModelQueryExpectedResults.empty
        |> ModelQueryExpectedResults.number_models
      in
      match logging_group_name with
      | None ->
          let () =
            Statistics.log_model_query_outputs
              ~is_group:false
              ~model_query_name:(ModelQuery.unique_identifier query)
              ~generated_models_count:models_count
              ()
          in
          let errors =
            if models_count = 0 then
              { ModelVerificationError.kind = NoOutputFromModelQuery name; location; path }
              :: errors
            else
              errors
          in
          logging_group_map, errors
      | Some logging_group_name ->
          let update = function
            | None -> { LoggingGroup.models_count; location; path }
            | Some existing -> LoggingGroup.add existing models_count
          in
          let logging_group_map = Map.update ~f:update logging_group_map logging_group_name in
          logging_group_map, errors
    in
    let check_logging_group
        ~key:logging_group_name
        ~data:{ LoggingGroup.models_count; location; path; _ }
        errors
      =
      let () =
        Statistics.log_model_query_outputs
          ~is_group:true
          ~model_query_name:logging_group_name
          ~generated_models_count:models_count
          ()
      in
      if models_count = 0 then
        {
          ModelVerificationError.kind = NoOutputFromModelQueryGroup logging_group_name;
          location;
          path;
        }
        :: errors
      else
        errors
    in
    (* Exclude `WriteToCache` queries since they don't produce models. *)
    let queries =
      List.filter
        ~f:(fun { ModelQuery.models; _ } ->
          not (List.exists ~f:ModelQuery.Model.is_write_to_cache models))
        queries
    in
    let logging_group_map, errors =
      List.fold ~f:count_models ~init:(String.Map.empty, []) queries
    in
    let errors = Map.fold logging_group_map ~f:check_logging_group ~init:errors in
    Statistics.flush ();
    errors
end

module ExecutionResult = struct
  type t = {
    shared_models: SharedModels.AddOnly.t;
    errors: ModelVerificationError.t list;
    (* Actual results for expected and unexpected models *)
    models_for_expected: ModelQueriesExpectedResults.t;
  }

  let create_empty () =
    {
      shared_models = SharedModels.create () |> SharedModels.add_only;
      errors = [];
      models_for_expected = ModelQueriesExpectedResults.empty;
    }


  let create_accumulator { shared_models; _ } =
    {
      shared_models = SharedModels.AddOnly.create_empty shared_models;
      errors = [];
      models_for_expected = ModelQueriesExpectedResults.empty;
    }


  let get_models { shared_models; _ } = SharedModels.from_add_only shared_models

  let get_errors { errors; _ } = errors

  let merge_disjoint
      ~smaller:
        {
          shared_models = smaller_shared_models;
          errors = smaller_errors;
          models_for_expected = smaller_models_for_expected;
        }
      ~larger:
        {
          shared_models = larger_shared_models;
          errors = larger_errors;
          models_for_expected = larger_models_for_expected;
        }
    =
    {
      shared_models =
        SharedModels.AddOnly.merge_same_handle_disjoint_keys
          ~smaller:smaller_shared_models
          ~larger:larger_shared_models;
      errors = List.append smaller_errors larger_errors;
      models_for_expected =
        ModelQueriesExpectedResults.merge_disjoint
          smaller_models_for_expected
          larger_models_for_expected;
    }


  let add_errors { shared_models; errors; models_for_expected } new_errors =
    { shared_models; errors = List.append errors new_errors; models_for_expected }


  let get_number_models { shared_models; _ } =
    shared_models |> SharedModels.AddOnly.keys |> List.length


  let add_models_for_new_target
      { shared_models; errors; models_for_expected }
      ~target
      ~queries
      ~models
    =
    let models = Map.filter_map models ~f:(fun registry -> Registry.get registry target) in
    let model =
      Map.fold
        ~init:None
        ~f:(fun ~key:_ ~data sofar -> Option.merge sofar (Some data) ~f:Model.join_user_models)
        models
    in
    match model with
    | None -> { shared_models; errors; models_for_expected }
    | Some model ->
        let shared_models = SharedModels.AddOnly.add shared_models target model in
        let models_for_expected =
          List.fold queries ~init:models_for_expected ~f:(fun models_for_expected query ->
              match Map.find models (ModelQuery.unique_identifier query) with
              | Some model ->
                  ModelQueriesExpectedResults.add_model_for_new_target
                    models_for_expected
                    ~target
                    ~query
                    ~model
              | None -> models_for_expected)
        in
        { shared_models; errors; models_for_expected }


  let add_models_for_new_queries
      { shared_models; errors; models_for_expected }
      ~queries
      model_query_results
    =
    let shared_models =
      model_query_results
      |> ModelQueryResultMap.get_registry ~model_join:Model.join_user_models
      |> SharedModels.join_with_registry_sequential
           (SharedModels.from_add_only shared_models)
           ~model_join:Model.join_user_models
      |> SharedModels.add_only
    in
    let models_for_expected =
      ModelQueriesExpectedResults.add_models_for_new_queries
        models_for_expected
        ~queries
        model_query_results
    in
    { shared_models; errors; models_for_expected }


  let check_expected_and_unexpected_model_errors
      ~queries
      { shared_models; errors; models_for_expected }
    =
    let new_errors =
      ModelQueriesExpectedResults.check_expected_and_unexpected_model_errors
        models_for_expected
        ~queries
    in
    { shared_models; errors = List.append errors new_errors; models_for_expected }


  let errors_for_queries_without_output ~queries { shared_models; errors; models_for_expected } =
    let new_errors =
      ModelQueriesExpectedResults.errors_for_queries_without_output models_for_expected ~queries
    in
    { shared_models; errors = List.append errors new_errors; models_for_expected }


  let dump_to_string { shared_models; _ } =
    let model_to_json (callable, model) =
      Model.to_json
        ~expand_overrides:None
        ~is_valid_callee:(fun ~trace_kind:_ ~port:_ ~path:_ ~callee:_ -> true)
        ~resolve_module_path:None
        ~resolve_callable_location:None
        ~export_leaf_names:Domains.ExportLeafNames.Always
        callable
        model
    in
    let to_json ~key:model_query_identifier ~data:models =
      models
      |> Registry.to_alist
      |> List.map ~f:model_to_json
      |> fun models ->
      `List models |> fun models_json -> `Assoc [model_query_identifier, models_json]
    in
    let build_query_to_registry_map ~target ~model:({ Model.model_generators; _ } as model) map =
      model_generators
      |> Model.ModelGeneratorSet.elements
      |> List.fold ~init:map ~f:(fun map model_query_identifier ->
             Map.update map model_query_identifier ~f:(function
                 | None -> Registry.singleton ~target ~model
                 | Some existing ->
                     Registry.add ~join:(fun _ _ -> failwith "non-disjoint") existing ~target ~model))
    in
    let map =
      SharedModels.fold_sequential
        (SharedModels.from_add_only shared_models)
        ~init:String.Map.empty
        ~f:build_query_to_registry_map
    in
    `List (Map.data (Map.mapi map ~f:to_json)) |> Yojson.Safe.pretty_to_string


  let dump_to_file model_query_results ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    path |> File.create ~content:(dump_to_string model_query_results) |> File.write


  let dump_to_file_and_string model_query_results ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    let content = dump_to_string model_query_results in
    path |> File.create ~content |> File.write;
    content
end

let sanitized_location_insensitive_compare left right =
  let sanitize_decorator_argument { Expression.Call.Argument.name; value } =
    let new_name =
      match name with
      | None -> None
      | Some ({ Node.value = argument_name; _ } as previous_name) ->
          Some { previous_name with value = Identifier.sanitized argument_name }
    in
    let new_value =
      match value with
      | { Node.value = Expression.Expression.Name (Expression.Name.Identifier argument_value); _ }
        as previous_value ->
          {
            previous_value with
            value =
              Expression.Expression.Name
                (Expression.Name.Identifier (Identifier.sanitized argument_value));
          }
      | _ -> value
    in
    { Expression.Call.Argument.name = new_name; value = new_value }
  in
  let left_sanitized = sanitize_decorator_argument left in
  let right_sanitized = sanitize_decorator_argument right in
  Expression.Call.Argument.location_insensitive_compare left_sanitized right_sanitized


module SanitizedCallArgumentSet = Set.Make (struct
  type t = Expression.Call.Argument.t [@@deriving sexp]

  let compare = sanitized_location_insensitive_compare
end)

let find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name =
  let rec find_children_transitive ~class_hierarchy_graph to_process result =
    match to_process with
    | [] -> result
    | class_name :: rest ->
        let child_name_set =
          ClassHierarchyGraph.SharedMemory.get ~class_name class_hierarchy_graph
        in
        let new_children = ClassHierarchyGraph.ClassNameSet.elements child_name_set in
        let result =
          List.fold ~f:(Fn.flip ClassHierarchyGraph.ClassNameSet.add) ~init:result new_children
        in
        find_children_transitive ~class_hierarchy_graph (List.rev_append new_children rest) result
  in
  let child_name_set =
    if is_transitive then
      match ClassHierarchyGraph.SharedMemory.get_transitive ~class_name class_hierarchy_graph with
      | Some child_name_set -> child_name_set
      (* cache miss, recalculate *)
      | None ->
          find_children_transitive
            ~class_hierarchy_graph
            [class_name]
            ClassHierarchyGraph.ClassNameSet.empty
    else
      ClassHierarchyGraph.SharedMemory.get ~class_name class_hierarchy_graph
  in
  let child_name_set =
    if includes_self then
      ClassHierarchyGraph.ClassNameSet.add class_name child_name_set
    else
      child_name_set
  in
  child_name_set


let matches_name_constraint ~name_captures ~name_constraint name =
  match name_constraint with
  | ModelQuery.NameConstraint.Equals string -> String.equal string name
  | ModelQuery.NameConstraint.Matches pattern ->
      let is_match = Re2.matches pattern name in
      let () =
        if is_match then
          NameCaptures.add name_captures (Re2.first_match_exn pattern name)
      in
      is_match


let matches_callee_constraint ~name_captures ~name_constraint callees =
  let { Interprocedural.CallGraph.CallCallees.call_targets; new_targets; init_targets; _ } =
    callees
  in
  let call_targets = call_targets |> List.rev_append new_targets |> List.rev_append init_targets in
  let call_target_to_string call_target =
    call_target
    |> CallGraph.CallTarget.target
    |> Interprocedural.Target.get_regular
    |> Interprocedural.Target.Regular.override_to_method
    |> Interprocedural.Target.Regular.define_name_exn
    |> Reference.show
  in
  List.exists call_targets ~f:(fun call_target ->
      matches_name_constraint ~name_captures ~name_constraint (call_target_to_string call_target))


let rec matches_decorator_constraint ~pyre_api ~name_captures ~decorator = function
  | ModelQuery.DecoratorConstraint.AnyOf constraints ->
      List.exists constraints ~f:(matches_decorator_constraint ~pyre_api ~name_captures ~decorator)
  | ModelQuery.DecoratorConstraint.AllOf constraints ->
      List.for_all constraints ~f:(matches_decorator_constraint ~pyre_api ~name_captures ~decorator)
  | ModelQuery.DecoratorConstraint.Not decorator_constraint ->
      not (matches_decorator_constraint ~pyre_api ~name_captures ~decorator decorator_constraint)
  | ModelQuery.DecoratorConstraint.NameConstraint name_constraint ->
      let { Statement.Decorator.name = { Node.value = decorator_name; _ }; _ } =
        CallableDecorator.statement decorator
      in
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (decorator_name |> Reference.delocalize |> Reference.last)
  | ModelQuery.DecoratorConstraint.FullyQualifiedCallee name_constraint ->
      let callees =
        match CallableDecorator.callees decorator with
        | Some callees -> callees
        | None ->
            (* This is forbidden during parsing *)
            failwith "fully_qualified_callee is not supported within cls.decorator"
      in
      matches_callee_constraint ~name_captures ~name_constraint callees
  | ModelQuery.DecoratorConstraint.ArgumentsConstraint arguments_constraint -> (
      let { Statement.Decorator.arguments = decorator_arguments; _ } =
        CallableDecorator.statement decorator
      in
      let split_arguments =
        List.partition_tf ~f:(fun { Expression.Call.Argument.name; _ } ->
            match name with
            | None -> true
            | _ -> false)
      in
      let positional_arguments_equal left right =
        List.equal (fun l r -> Int.equal (sanitized_location_insensitive_compare l r) 0) left right
      in
      match arguments_constraint, decorator_arguments with
      | ModelQuery.ArgumentsConstraint.Contains constraint_arguments, None ->
          List.is_empty constraint_arguments
      | ModelQuery.ArgumentsConstraint.Contains constraint_arguments, Some arguments ->
          let constraint_positional_arguments, constraint_keyword_arguments =
            split_arguments constraint_arguments
          in
          let decorator_positional_arguments, decorator_keyword_arguments =
            split_arguments arguments
          in
          List.length constraint_positional_arguments <= List.length decorator_positional_arguments
          && positional_arguments_equal
               constraint_positional_arguments
               (List.take
                  decorator_positional_arguments
                  (List.length constraint_positional_arguments))
          && Set.is_subset
               (SanitizedCallArgumentSet.of_list constraint_keyword_arguments)
               ~of_:(SanitizedCallArgumentSet.of_list decorator_keyword_arguments)
      | ModelQuery.ArgumentsConstraint.Equals constraint_arguments, None ->
          List.is_empty constraint_arguments
      | ModelQuery.ArgumentsConstraint.Equals constraint_arguments, Some arguments ->
          let constraint_positional_arguments, constraint_keyword_arguments =
            split_arguments constraint_arguments
          in
          let decorator_positional_arguments, decorator_keyword_arguments =
            split_arguments arguments
          in
          (* Since equality comparison is more costly, check the lists are the same lengths
             first. *)
          Int.equal
            (List.length constraint_positional_arguments)
            (List.length decorator_positional_arguments)
          && positional_arguments_equal
               constraint_positional_arguments
               decorator_positional_arguments
          && SanitizedCallArgumentSet.equal
               (SanitizedCallArgumentSet.of_list constraint_keyword_arguments)
               (SanitizedCallArgumentSet.of_list decorator_keyword_arguments))


module TypeAnnotation : sig
  type t

  val from_original_annotation
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    preserve_original:bool ->
    Expression.t ->
    t

  val from_pysa_type : PyrePysaApi.PysaType.t -> t

  val is_annotated : t -> bool

  val as_type : t -> PyrePysaApi.PysaType.t

  (* Show the original annotation, as written by the user. *)
  val show_original_annotation : t -> string

  (* Show the fully qualified type annotation from the type checker. *)
  val show_fully_qualified_annotation : t -> string
end = struct
  type t = {
    expression: Expression.t option;
        (* The original annotation, as an expression. None if not supported. *)
    type_: PyrePysaApi.PysaType.t Lazy.t;
  }

  let from_original_annotation ~pyre_api ~preserve_original expression =
    {
      expression = Option.some_if preserve_original expression;
      type_ =
        lazy
          (PyrePysaApi.ReadOnly.parse_annotation pyre_api expression
          |> PyrePysaApi.PysaType.from_pyre1_type);
    }


  let from_pysa_type type_ = { expression = None; type_ = lazy type_ }

  let is_annotated = function
    | { expression = None; _ } -> failwith "is_annotated is not supported in this context"
    | { expression = Some { Node.value = expression; _ }; _ } -> (
        match expression with
        | Expression.Expression.Subscript
            {
              base =
                { Node.value = Name (Expression.Name.Attribute { attribute = "Annotated"; _ }); _ };
              _;
            } ->
            true
        | _ -> false)


  let as_type { type_; _ } = Lazy.force type_

  (* Show the original annotation, as written by the user. *)
  let show_original_annotation = function
    | { expression = Some expression; _ } -> Expression.show expression
    | _ -> failwith "show_original_annotation is not supported in this context"


  (* Show the parsed annotation from pyre *)
  let show_fully_qualified_annotation { type_; _ } =
    PyrePysaApi.PysaType.show_fully_qualified (Lazy.force type_)
end

let matches_annotation_constraint
    ~pyre_api
    ~class_hierarchy_graph
    ~name_captures
    ~annotation_constraint
    annotation
  =
  match annotation_constraint with
  | ModelQuery.AnnotationConstraint.IsAnnotatedTypeConstraint ->
      TypeAnnotation.is_annotated annotation
  | ModelQuery.AnnotationConstraint.OriginalAnnotationConstraint name_constraint ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (TypeAnnotation.show_original_annotation annotation)
  | ModelQuery.AnnotationConstraint.FullyQualifiedConstraint name_constraint ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (TypeAnnotation.show_fully_qualified_annotation annotation)
  | ModelQuery.AnnotationConstraint.AnnotationClassExtends
      { class_name; is_transitive; includes_self } -> (
      match PyrePysaApi.PysaType.get_class_names (TypeAnnotation.as_type annotation) with
      | {
       PyrePysaApi.PysaType.ClassNamesResult.class_names = [extracted_class_name];
       is_exhaustive = true;
       _;
      } ->
          let class_name =
            Reference.show
              (PyrePysaApi.ReadOnly.add_builtins_prefix pyre_api (Reference.create class_name))
          in
          find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name
          |> ClassHierarchyGraph.ClassNameSet.mem extracted_class_name
      | _ -> false)


let rec parameter_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~parameter
  = function
  | ModelQuery.ParameterConstraint.AnnotationConstraint annotation_constraint ->
      FunctionParameter.annotation parameter
      >>| TypeAnnotation.from_pysa_type
      >>| matches_annotation_constraint
            ~pyre_api
            ~class_hierarchy_graph
            ~name_captures
            ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.ParameterConstraint.NameConstraint name_constraint ->
      FunctionParameter.name parameter
      >>| matches_name_constraint ~name_captures ~name_constraint
      |> Option.value ~default:false
  | ModelQuery.ParameterConstraint.IndexConstraint index -> (
      match parameter with
      | FunctionParameter.PositionalOnly { position = parameter_position; _ }
      | FunctionParameter.Named { position = parameter_position; _ } ->
          parameter_position = index
      | _ -> false)
  | ModelQuery.ParameterConstraint.HasPosition -> (
      match parameter with
      | FunctionParameter.PositionalOnly _
      | FunctionParameter.Named _ ->
          true
      | _ -> false)
  | ModelQuery.ParameterConstraint.HasName -> (
      match parameter with
      | FunctionParameter.PositionalOnly _
      | FunctionParameter.Named _
      | FunctionParameter.KeywordOnly _ ->
          true
      | _ -> false)
  | ModelQuery.ParameterConstraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:(parameter_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~parameter)
  | ModelQuery.ParameterConstraint.Not query_constraint ->
      not
        (parameter_matches_constraint
           ~pyre_api
           ~class_hierarchy_graph
           ~name_captures
           ~parameter
           query_constraint)
  | ModelQuery.ParameterConstraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:(parameter_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~parameter)


let class_matches_decorator_constraint ~name_captures ~pyre_api ~decorator_constraint class_name =
  PyrePysaApi.ReadOnly.get_class_summary pyre_api class_name
  >>| Node.value
  >>| (fun { decorators; _ } ->
        List.exists decorators ~f:(fun decorator ->
            Statement.Decorator.from_expression decorator
            >>| (fun decorator ->
                  (* For now, we don't support `fully_qualified_callee` within `cls.decorator()` constraints.
                   * We could do it in the future by storing the result of the call to `get_class_summary` above
                   * in `Modelable.t` *)
                  matches_decorator_constraint
                    ~pyre_api
                    ~name_captures
                    ~decorator:(CallableDecorator.create_without_callees decorator)
                    decorator_constraint)
            |> Option.value ~default:false))
  |> Option.value ~default:false


let find_parents ~pyre_api ~is_transitive ~includes_self class_name =
  let parents =
    if is_transitive then (* TODO(T225700656): implement successors for pyrefly *)
      PyrePysaApi.ReadOnly.successors pyre_api class_name
    else
      PyrePysaApi.ReadOnly.class_immediate_parents pyre_api class_name
  in
  let parents =
    if includes_self then
      class_name :: parents
    else
      parents
  in
  parents


let find_base_methods
    ~pyre_api
    ~callables_to_definitions_map
    { Target.Method.class_name; method_name; kind }
  =
  let find_instance_method parent_class =
    let base_method = Target.create_method ~kind (Reference.create parent_class) method_name in
    match
      Target.CallablesSharedMemory.ReadOnly.get_signature callables_to_definitions_map base_method
    with
    | Some { Target.CallableSignature.method_kind = Some Target.MethodKind.Instance; _ } ->
        Some base_method
    | _ -> None
  in
  find_parents ~pyre_api ~is_transitive:true ~includes_self:false class_name
  |> List.filter_map ~f:find_instance_method


let rec class_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~name = function
  | ModelQuery.ClassConstraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:(class_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~name)
  | ModelQuery.ClassConstraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:(class_matches_constraint ~pyre_api ~class_hierarchy_graph ~name_captures ~name)
  | ModelQuery.ClassConstraint.Not class_constraint ->
      not
        (class_matches_constraint
           ~pyre_api
           ~name
           ~class_hierarchy_graph
           ~name_captures
           class_constraint)
  | ModelQuery.ClassConstraint.NameConstraint name_constraint ->
      let name =
        name
        |> Reference.create
        |> PyrePysaApi.ReadOnly.target_symbolic_name pyre_api
        |> Reference.last
      in
      matches_name_constraint ~name_captures ~name_constraint name
  | ModelQuery.ClassConstraint.FullyQualifiedNameConstraint name_constraint ->
      let name =
        name
        |> Reference.create
        |> PyrePysaApi.ReadOnly.target_symbolic_name pyre_api
        |> Reference.show
      in
      matches_name_constraint ~name_captures ~name_constraint name
  | ModelQuery.ClassConstraint.Extends { class_name; is_transitive; includes_self } ->
      find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name
      |> ClassHierarchyGraph.ClassNameSet.mem name
  | ModelQuery.ClassConstraint.DecoratorConstraint decorator_constraint ->
      class_matches_decorator_constraint ~name_captures ~pyre_api ~decorator_constraint name
  | ModelQuery.ClassConstraint.AnyChildConstraint { class_constraint; is_transitive; includes_self }
    ->
      find_children ~class_hierarchy_graph ~is_transitive ~includes_self name
      |> ClassHierarchyGraph.ClassNameSet.exists (fun name ->
             class_matches_constraint
               ~pyre_api
               ~name
               ~class_hierarchy_graph
               ~name_captures
               class_constraint)
  | ModelQuery.ClassConstraint.AnyParentConstraint
      { class_constraint; is_transitive; includes_self } ->
      find_parents ~pyre_api ~is_transitive ~includes_self name
      |> List.exists ~f:(fun name ->
             class_matches_constraint
               ~pyre_api
               ~name
               ~class_hierarchy_graph
               ~name_captures
               class_constraint)


let rec matches_constraint
    ~pyre_api
    ~callables_to_definitions_map
    ~class_hierarchy_graph
    ~name_captures
    value
    query_constraint
  =
  match query_constraint with
  | ModelQuery.Constraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:
          (matches_constraint
             ~pyre_api
             ~callables_to_definitions_map
             ~class_hierarchy_graph
             ~name_captures
             value)
  | ModelQuery.Constraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:
          (matches_constraint
             ~pyre_api
             ~callables_to_definitions_map
             ~class_hierarchy_graph
             ~name_captures
             value)
  | ModelQuery.Constraint.Not query_constraint ->
      not
        (matches_constraint
           ~pyre_api
           ~callables_to_definitions_map
           ~class_hierarchy_graph
           ~name_captures
           value
           query_constraint)
  | ModelQuery.Constraint.Constant value -> value
  | ModelQuery.Constraint.NameConstraint name_constraint ->
      let name =
        value
        |> Modelable.target_name
        |> PyrePysaApi.ReadOnly.target_symbolic_name pyre_api
        |> Reference.last
      in
      matches_name_constraint ~name_captures ~name_constraint name
  | ModelQuery.Constraint.FullyQualifiedNameConstraint name_constraint ->
      let name =
        value
        |> Modelable.target_name
        |> PyrePysaApi.ReadOnly.target_symbolic_name pyre_api
        |> Reference.show
      in
      matches_name_constraint ~name_captures ~name_constraint name
  | ModelQuery.Constraint.AnnotationConstraint annotation_constraint ->
      Modelable.type_annotation value
      >>| TypeAnnotation.from_original_annotation ~pyre_api ~preserve_original:true
      >>| matches_annotation_constraint
            ~pyre_api
            ~class_hierarchy_graph
            ~name_captures
            ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.Constraint.ReturnConstraint annotation_constraint ->
      Modelable.return_annotations value
      |> List.exists ~f:(fun return_annotation ->
             return_annotation
             |> TypeAnnotation.from_pysa_type
             |> matches_annotation_constraint
                  ~pyre_api
                  ~class_hierarchy_graph
                  ~name_captures
                  ~annotation_constraint)
  | ModelQuery.Constraint.AnyParameterConstraint parameter_constraint ->
      Modelable.parameters_of_signatures value
      |> List.exists ~f:(fun parameter ->
             parameter_matches_constraint
               ~pyre_api
               ~class_hierarchy_graph
               ~name_captures
               ~parameter
               parameter_constraint)
  | ModelQuery.Constraint.AnyOverridenMethod constraint_ -> (
      match value |> Modelable.target |> Target.get_regular with
      | Target.Regular.Method method_name when Modelable.is_instance_method value ->
          find_base_methods ~pyre_api ~callables_to_definitions_map method_name
          |> List.exists ~f:(fun base_method ->
                 matches_constraint
                   ~pyre_api
                   ~callables_to_definitions_map
                   ~class_hierarchy_graph
                   ~name_captures
                   (Modelable.create_callable ~pyre_api ~callables_to_definitions_map base_method)
                   constraint_)
      | _ -> false)
  | ModelQuery.Constraint.ReadFromCache _ ->
      (* This is handled before matching constraints. *)
      true
  | ModelQuery.Constraint.AnyDecoratorConstraint decorator_constraint ->
      Modelable.resolved_original_decorators value
      |> List.exists ~f:(fun decorator ->
             matches_decorator_constraint ~pyre_api ~name_captures ~decorator decorator_constraint)
  | ModelQuery.Constraint.ClassConstraint class_constraint ->
      Modelable.class_name value
      >>| (fun name ->
            class_matches_constraint
              ~pyre_api
              ~class_hierarchy_graph
              ~name_captures
              ~name
              class_constraint)
      |> Option.value ~default:false


module PartitionTargetQueries = struct
  type t = {
    callable_queries: ModelQuery.t list;
    attribute_queries: ModelQuery.t list;
    global_queries: ModelQuery.t list;
  }

  let partition queries =
    let attribute_queries, global_queries, callable_queries =
      List.partition3_map
        ~f:(fun query ->
          match query.ModelQuery.find with
          | ModelQuery.Find.Attribute -> `Fst query
          | ModelQuery.Find.Global -> `Snd query
          | _ -> `Trd query)
        queries
    in
    { callable_queries; attribute_queries; global_queries }
end

module PartitionCacheQueries = struct
  type t = {
    write_to_cache: ModelQuery.t list;
    read_from_cache: ModelQuery.t list;
    others: ModelQuery.t list;
  }
  [@@deriving show, equal]

  let empty = { write_to_cache = []; read_from_cache = []; others = [] }

  let add_read_from_cache query partition =
    { partition with read_from_cache = query :: partition.read_from_cache }


  let add_write_to_cache query partition =
    { partition with write_to_cache = query :: partition.write_to_cache }


  let add_others query partition = { partition with others = query :: partition.others }

  let partition queries =
    let add partition ({ ModelQuery.where; models; _ } as query) =
      if ModelQuery.Constraint.contains_read_from_cache (AllOf where) then
        add_read_from_cache query partition
      else if List.exists ~f:ModelQuery.Model.is_write_to_cache models then
        add_write_to_cache query partition
      else
        add_others query partition
    in
    List.fold ~init:empty ~f:add queries
end

(* This is the cache for `WriteToCache` and `read_from_cache` *)
module ReadWriteCache = struct
  module NameToTargetSet = struct
    type t = Target.Set.t SerializableStringMap.t

    let empty = SerializableStringMap.empty

    let singleton ~name ~target =
      SerializableStringMap.add name (Target.Set.singleton target) SerializableStringMap.empty


    let write map ~name ~target =
      SerializableStringMap.update
        name
        (function
          | None -> Some (Target.Set.singleton target)
          | Some targets -> Some (Target.Set.add target targets))
        map


    let read map ~name =
      SerializableStringMap.find_opt name map |> Option.value ~default:Target.Set.empty


    let merge = SerializableStringMap.merge (fun _ -> Option.merge ~f:Target.Set.union)
  end

  type t = NameToTargetSet.t SerializableStringMap.t

  let empty = SerializableStringMap.empty

  let write map ~kind ~name ~target =
    SerializableStringMap.update
      kind
      (function
        | None -> Some (NameToTargetSet.singleton ~name ~target)
        | Some name_targets -> Some (NameToTargetSet.write name_targets ~name ~target))
      map


  let read map ~kind ~name =
    SerializableStringMap.find_opt kind map
    |> Option.value ~default:NameToTargetSet.empty
    |> NameToTargetSet.read ~name


  let merge = SerializableStringMap.merge (fun _ -> Option.merge ~f:NameToTargetSet.merge)

  let show_set set =
    set
    |> Target.Set.elements
    |> List.map ~f:Target.external_name
    |> String.concat ~sep:", "
    |> Format.sprintf "{%s}"


  let pp_set formatter set = Format.fprintf formatter "%s" (show_set set)

  let pp = SerializableStringMap.pp (SerializableStringMap.pp pp_set)

  let show = Format.asprintf "%a" pp

  let equal = SerializableStringMap.equal (SerializableStringMap.equal Target.Set.equal)
end

module CandidateTargetsFromCache = struct
  type t =
    | Top
    | Set of Target.Set.t
  [@@deriving equal]

  let bottom = Set Target.Set.empty

  let meet left right =
    match left, right with
    | Top, _ -> right
    | _, Top -> left
    | Set left, Set right -> Set (Target.Set.inter left right)


  let join left right =
    match left, right with
    | Top, _
    | _, Top ->
        Top
    | Set left, Set right -> Set (Target.Set.union left right)


  let rec from_constraint cache = function
    | ModelQuery.Constraint.ReadFromCache { kind; name } ->
        Set (ReadWriteCache.read cache ~name ~kind)
    | ModelQuery.Constraint.AnyOf constraints ->
        List.fold
          ~init:bottom
          ~f:(fun candidates constraint_ -> join candidates (from_constraint cache constraint_))
          constraints
    | ModelQuery.Constraint.AllOf constraints ->
        List.fold
          ~init:Top
          ~f:(fun candidates constraint_ -> meet candidates (from_constraint cache constraint_))
          constraints
    | ModelQuery.Constraint.Constant false -> Set Target.Set.empty
    | ModelQuery.Constraint.Constant true
    | ModelQuery.Constraint.Not _
    | ModelQuery.Constraint.NameConstraint _
    | ModelQuery.Constraint.FullyQualifiedNameConstraint _
    | ModelQuery.Constraint.AnnotationConstraint _
    | ModelQuery.Constraint.ReturnConstraint _
    | ModelQuery.Constraint.AnyParameterConstraint _
    | ModelQuery.Constraint.AnyDecoratorConstraint _
    | ModelQuery.Constraint.ClassConstraint _
    | ModelQuery.Constraint.AnyOverridenMethod _ ->
        Top


  let pp formatter = function
    | Top -> Format.fprintf formatter "Top"
    | Set set -> Format.fprintf formatter "Set(%s)" (ReadWriteCache.show_set set)


  let show = Format.asprintf "%a" pp
end

(* Module interface that we need to provide for each type of query (callable, attribute and
   global). *)
module type QUERY_KIND = sig
  (* The type of annotation produced by this type of query (e.g, `ModelAnnotation.t` for callables
     and `TaintAnnotation.t` for attributes and globals). *)
  type annotation

  val query_kind_name : string

  (* When using multiprocessing, this is the name of the scheduler. *)
  val schedule_identifier : Configuration.ScheduleIdentifier.t

  val make_modelable
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    callables_to_definitions_map:Interprocedural.Target.CallablesSharedMemory.ReadOnly.t ->
    Target.t ->
    Modelable.t

  (* Generate taint annotations from the `models` part of a given model query. *)
  val generate_annotations_from_query_models
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    class_hierarchy_graph:ClassHierarchyGraph.SharedMemory.t ->
    name_captures:NameCaptures.t ->
    modelable:Modelable.t ->
    ModelQuery.Model.t list ->
    annotation list

  val generate_model_from_annotations
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    source_sink_filter:SourceSinkFilter.t option ->
    stubs:Target.HashsetSharedMemory.ReadOnly.t ->
    target:Target.t ->
    modelable:Modelable.t ->
    annotation list ->
    (Model.t, ModelVerificationError.t) result
end

(* Functor that implements the generic logic that generates models from queries. *)
module MakeQueryExecutor (QueryKind : QUERY_KIND) = struct
  include QueryKind

  let matches_query_constraints
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~name_captures
      ~modelable
      { ModelQuery.find; where; name = query_name; _ }
    =
    let result =
      Modelable.matches_find modelable find
      && List.for_all
           ~f:
             (matches_constraint
                ~pyre_api
                ~callables_to_definitions_map
                ~class_hierarchy_graph
                ~name_captures
                modelable)
           where
    in
    let () =
      if verbose && result then
        Log.info
          "Target `%a` matches all constraints for the model query `%s`."
          Target.pp_pretty
          (Modelable.target modelable)
          query_name
    in
    result


  let generate_annotations_from_query_on_target
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~modelable
      ({ ModelQuery.models; _ } as query)
    =
    let name_captures = NameCaptures.create () in
    if
      matches_query_constraints
        ~verbose
        ~pyre_api
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~name_captures
        ~modelable
        query
    then
      QueryKind.generate_annotations_from_query_models
        ~pyre_api
        ~class_hierarchy_graph
        ~name_captures
        ~modelable
        models
    else
      []


  let generate_model_from_query_on_target
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~target
      ~modelable
      query
    =
    match
      generate_annotations_from_query_on_target
        ~verbose
        ~pyre_api
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~modelable
        query
    with
    | [] -> Ok None
    | annotations ->
        QueryKind.generate_model_from_annotations
          ~pyre_api
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          annotations
        |> Result.map ~f:(fun model ->
               Some
                 {
                   model with
                   Model.model_generators =
                     Model.ModelGeneratorSet.singleton (ModelQuery.unique_identifier query);
                 })


  let generate_models_from_query_on_targets
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      query
    =
    let fold (registry, errors) target =
      let modelable = QueryKind.make_modelable ~pyre_api ~callables_to_definitions_map target in
      match
        generate_model_from_query_on_target
          ~verbose
          ~pyre_api
          ~callables_to_definitions_map
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          query
      with
      | Ok (Some model) ->
          let registry = Registry.add registry ~join:Model.join_user_models ~target ~model in
          registry, errors
      | Ok None -> registry, errors
      | Error error -> registry, error :: errors
    in
    List.fold targets ~init:(Registry.empty, []) ~f:fold


  let generate_models_from_queries_on_target
      ~verbose
      ~pyre_api
      ~class_hierarchy_graph
      ~callables_to_definitions_map
      ~source_sink_filter
      ~stubs
      ~queries
      target
    =
    let modelable = QueryKind.make_modelable ~pyre_api ~callables_to_definitions_map target in
    let fold (current_models, current_errors) query =
      match
        generate_model_from_query_on_target
          ~verbose
          ~pyre_api
          ~callables_to_definitions_map
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          query
      with
      | Ok (Some model) ->
          let current_models =
            ModelQueryResultMap.add_model
              current_models
              ~model_join:(fun _ _ -> failwith "non-disjoint")
              ~query
              ~target
              ~model
          in
          current_models, current_errors
      | Ok None -> current_models, current_errors
      | Error error -> current_models, error :: current_errors
    in
    List.fold queries ~init:(ModelQueryResultMap.empty, []) ~f:fold


  let generate_models_from_queries_on_targets
      ~verbose
      ~pyre_api
      ~class_hierarchy_graph
      ~callables_to_definitions_map
      ~source_sink_filter
      ~stubs
      ~targets
      ~queries
      accumulator
    =
    List.fold
      ~init:accumulator
      ~f:(fun accumulator target ->
        let models, errors =
          generate_models_from_queries_on_target
            ~verbose
            ~pyre_api
            ~class_hierarchy_graph
            ~callables_to_definitions_map
            ~source_sink_filter
            ~stubs
            ~queries
            target
        in
        let accumulator =
          ExecutionResult.add_models_for_new_target accumulator ~target ~queries ~models
        in
        let accumulator = ExecutionResult.add_errors accumulator errors in
        accumulator)
      targets


  let generate_cache_from_query_on_target
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~initial_cache
      ~target
      ({ ModelQuery.models; name; _ } as query)
    =
    let name_captures = NameCaptures.create () in
    let modelable = QueryKind.make_modelable ~pyre_api ~callables_to_definitions_map target in
    let write_to_cache cache = function
      | ModelQuery.Model.WriteToCache { kind; name } -> (
          match Modelable.expand_format_string ~name_captures ~parameter:None modelable name with
          | Ok name -> ReadWriteCache.write cache ~kind ~name ~target
          | Error error -> Format.asprintf "unexpected WriteToCache name: %s" error |> failwith)
      | model ->
          Format.asprintf
            "unexpected model in generate_cache_from_query_on_target for model query `%s`, \
             expecting `WriteToCache`, got `%a`"
            name
            ModelQuery.Model.pp
            model
          |> failwith
    in
    if
      matches_query_constraints
        ~verbose
        ~pyre_api
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~name_captures
        ~modelable
        query
    then
      List.fold ~init:initial_cache ~f:write_to_cache models
    else
      initial_cache


  let generate_cache_from_queries_on_targets
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~targets
      write_to_cache_queries
    =
    let fold_target ~query cache target =
      generate_cache_from_query_on_target
        ~verbose
        ~pyre_api
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~initial_cache:cache
        ~target
        query
    in
    let fold_query cache query = List.fold targets ~init:cache ~f:(fold_target ~query) in
    List.fold write_to_cache_queries ~init:ReadWriteCache.empty ~f:fold_query


  let generate_cache_from_queries_on_targets_with_multiprocessing
      ~verbose
      ~pyre_api
      ~scheduler
      ~scheduler_policies
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~targets
    = function
    | [] -> ReadWriteCache.empty
    | write_to_cache_queries ->
        let map targets =
          generate_cache_from_queries_on_targets
            ~verbose
            ~pyre_api
            ~class_hierarchy_graph
            ~callables_to_definitions_map
            ~targets
            write_to_cache_queries
        in
        let scheduler_policy =
          Scheduler.Policy.from_configuration_or_default
            scheduler_policies
            QueryKind.schedule_identifier
            ~default:
              (Scheduler.Policy.fixed_chunk_count
                 ~minimum_chunks_per_worker:1
                 ~minimum_chunk_size:1
                 ~preferred_chunks_per_worker:1
                 ())
        in
        Scheduler.map_reduce
          scheduler
          ~policy:scheduler_policy
          ~initial:ReadWriteCache.empty
          ~map
          ~reduce:ReadWriteCache.merge
          ~inputs:targets
          ()


  let generate_models_from_read_cache_queries_on_targets
      ~verbose
      ~pyre_api
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~cache
      read_from_cache_queries
    =
    let fold
        (current_models, current_errors)
        ({ ModelQuery.name = model_query_name; where; _ } as query)
      =
      match CandidateTargetsFromCache.from_constraint cache (AllOf where) with
      | Top ->
          (* This should never happen, since model verification prevents building invalid
             read_from_cache queries. *)
          Format.sprintf
            "Model query `%s` has an invalid `read_from_cache` query: could not compute a set of \
             candidate targets"
            model_query_name
          |> failwith
      | Set candidates ->
          let new_models, new_errors =
            generate_models_from_query_on_targets
              ~verbose
              ~pyre_api
              ~callables_to_definitions_map
              ~class_hierarchy_graph
              ~source_sink_filter
              ~stubs
              ~targets:(Target.Set.elements candidates)
              query
          in
          ( ModelQueryResultMap.add_registry
              current_models
              ~model_join:Model.join_user_models
              ~query
              new_models,
            List.rev_append new_errors current_errors )
    in
    List.fold read_from_cache_queries ~init:(ModelQueryResultMap.empty, []) ~f:fold


  (* Generate models from non-cache queries. *)
  let generate_models_from_regular_queries_on_targets_with_multiprocessing
      ~verbose
      ~pyre_api
      ~scheduler
      ~scheduler_policies
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      accumulator
    = function
    | [] -> accumulator
    | regular_queries ->
        let empty_accumulator = ExecutionResult.create_accumulator accumulator in
        let map targets =
          generate_models_from_queries_on_targets
            ~verbose
            ~pyre_api
            ~class_hierarchy_graph
            ~callables_to_definitions_map
            ~source_sink_filter
            ~stubs
            ~targets
            ~queries:regular_queries
            empty_accumulator
        in
        let scheduler_policy =
          Scheduler.Policy.from_configuration_or_default
            scheduler_policies
            QueryKind.schedule_identifier
            ~default:
              (Scheduler.Policy.fixed_chunk_size
                 ~minimum_chunks_per_worker:1
                 ~minimum_chunk_size:1
                 ~preferred_chunk_size:5000
                 ())
        in
        Scheduler.map_reduce
          scheduler
          ~policy:scheduler_policy
          ~initial:accumulator
          ~map
          ~reduce:(fun left right -> ExecutionResult.merge_disjoint ~smaller:left ~larger:right)
          ~inputs:targets
          ()


  let generate_models_from_queries_on_targets_with_multiprocessing
      ~verbose
      ~pyre_api
      ~scheduler
      ~scheduler_policies
      ~callables_to_definitions_map
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      ~queries
      execution_result
    =
    let {
      PartitionCacheQueries.write_to_cache = write_to_cache_queries;
      read_from_cache = read_from_cache_queries;
      others = regular_queries;
    }
      =
      PartitionCacheQueries.partition queries
    in

    let execution_result =
      let () =
        Log.info
          "Generating models from %d regular %s model queries..."
          (List.length regular_queries)
          QueryKind.query_kind_name
      in
      let previous_size = ExecutionResult.get_number_models execution_result in
      let execution_result =
        generate_models_from_regular_queries_on_targets_with_multiprocessing
          ~verbose
          ~pyre_api
          ~scheduler
          ~scheduler_policies
          ~callables_to_definitions_map
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~targets
          execution_result
          regular_queries
      in
      let () =
        Log.info
          "Generated %d models from regular %s model queries."
          (ExecutionResult.get_number_models execution_result - previous_size)
          QueryKind.query_kind_name
      in
      execution_result
    in

    let execution_result =
      let () =
        Log.info
          "Building cache for %d %s model queries..."
          (List.length write_to_cache_queries)
          QueryKind.query_kind_name
      in
      let cache =
        generate_cache_from_queries_on_targets_with_multiprocessing
          ~verbose
          ~pyre_api
          ~scheduler
          ~scheduler_policies
          ~callables_to_definitions_map
          ~class_hierarchy_graph
          ~targets
          write_to_cache_queries
      in
      let () =
        Log.info
          "Generating models from %d cached %s model queries..."
          (List.length read_from_cache_queries)
          QueryKind.query_kind_name
      in
      let models, errors =
        generate_models_from_read_cache_queries_on_targets
          ~verbose
          ~pyre_api
          ~callables_to_definitions_map
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~cache
          read_from_cache_queries
      in
      let () =
        Log.info
          "Generated %d models from cached %s model queries."
          (ModelQueryResultMap.number_models models)
          QueryKind.query_kind_name
      in
      let execution_result = ExecutionResult.add_errors execution_result errors in
      let execution_result =
        ExecutionResult.add_models_for_new_queries
          execution_result
          ~queries:read_from_cache_queries
          models
      in
      execution_result
    in

    execution_result
end

module CallableQueryExecutor = MakeQueryExecutor (struct
  type annotation = ModelAnnotation.t

  let query_kind_name = "callable"

  let schedule_identifier = Configuration.ScheduleIdentifier.CallableModelQueries

  let make_modelable = Modelable.create_callable

  let generate_annotations_from_query_models
      ~pyre_api
      ~class_hierarchy_graph
      ~name_captures
      ~modelable
      models
    =
    let production_to_taint ~root ~production =
      let update_placeholder_via_feature ~actual_parameter =
        (* If we see a via_feature on the $global attribute symbolic parameter in the taint for an
           actual parameter, we replace it with the actual parameter. *)
        let open Features in
        function
        | ViaFeature.ViaTypeOf
            {
              parameter =
                AccessPath.Root.PositionalParameter
                  { position = 0; name = "$global"; positional_only = false };
              tag;
            } ->
            ViaFeature.ViaTypeOf { parameter = actual_parameter; tag }
        | ViaFeature.ViaValueOf
            {
              parameter =
                AccessPath.Root.PositionalParameter
                  { position = 0; name = "$global"; positional_only = false };
              tag;
            } ->
            ViaFeature.ViaValueOf { parameter = actual_parameter; tag }
        | feature -> feature
      in
      let update_placeholder_via_features taint_annotation =
        if AccessPath.Root.is_parameter root then
          match taint_annotation with
          | ModelParseResult.TaintAnnotation.Source { source; features } ->
              let via_features =
                List.map
                  ~f:(update_placeholder_via_feature ~actual_parameter:root)
                  features.via_features
              in
              ModelParseResult.TaintAnnotation.Source
                { source; features = { features with via_features } }
          | ModelParseResult.TaintAnnotation.Sink { sink; features } ->
              let via_features =
                List.map
                  ~f:(update_placeholder_via_feature ~actual_parameter:root)
                  features.via_features
              in
              ModelParseResult.TaintAnnotation.Sink
                { sink; features = { features with via_features } }
          | ModelParseResult.TaintAnnotation.Tito { tito; features } ->
              let via_features =
                List.map
                  ~f:(update_placeholder_via_feature ~actual_parameter:root)
                  features.via_features
              in
              ModelParseResult.TaintAnnotation.Tito
                { tito; features = { features with via_features } }
          | ModelParseResult.TaintAnnotation.AddFeatureToArgument { features } ->
              let via_features =
                List.map
                  ~f:(update_placeholder_via_feature ~actual_parameter:root)
                  features.via_features
              in
              ModelParseResult.TaintAnnotation.AddFeatureToArgument
                { features = { features with via_features } }
          | _ -> taint_annotation
        else
          taint_annotation
      in
      match production with
      | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation ->
          Some (update_placeholder_via_features taint_annotation)
      | ModelQuery.QueryTaintAnnotation.CrossRepositoryTaintAnchor
          { annotation; canonical_name; canonical_port } ->
          let expand_format_string format_string =
            match
              Modelable.expand_format_string
                ~name_captures
                ~parameter:(Some root)
                modelable
                format_string
            with
            | Ok name -> name
            | Error error ->
                Format.asprintf "unexpected CrossRepositoryTaintAnchor argument: %s" error
                |> failwith
          in
          annotation
          |> update_placeholder_via_features
          |> TaintAnnotation.add_cross_repository_anchor
               ~canonical_name:(expand_format_string canonical_name)
               ~canonical_port:(expand_format_string canonical_port)
          |> Option.some
    in
    let apply_model ~parameters ~captures = function
      | ModelQuery.Model.Return productions ->
          List.filter_map productions ~f:(fun production ->
              production_to_taint ~root:AccessPath.Root.LocalResult ~production
              >>| fun taint -> ModelParseResult.ModelAnnotation.ReturnAnnotation taint)
      | ModelQuery.Model.CapturedVariables { taint = productions; generation_if_source } ->
          List.cartesian_product productions captures
          |> List.filter_map ~f:(fun (production, capture) ->
                 let root =
                   AccessPath.Root.CapturedVariable { name = capture.Statement.Define.Capture.name }
                 in
                 production_to_taint ~root ~production
                 >>| fun annotation ->
                 ModelParseResult.ModelAnnotation.ParameterAnnotation
                   { root; annotation; generation_if_source })
      | ModelQuery.Model.NamedParameter { name; taint = productions } -> (
          let parameter =
            List.find_map parameters ~f:(fun parameter ->
                if Option.equal String.equal (FunctionParameter.name parameter) (Some name) then
                  Some (FunctionParameter.root parameter)
                else
                  None)
          in
          match parameter with
          | Some parameter ->
              List.filter_map productions ~f:(fun production ->
                  production_to_taint ~root:parameter ~production
                  >>| fun annotation ->
                  ModelParseResult.ModelAnnotation.ParameterAnnotation
                    { root = parameter; annotation; generation_if_source = false })
          | None -> [])
      | ModelQuery.Model.PositionalParameter { index; taint = productions } -> (
          let parameter =
            List.find_map parameters ~f:(fun parameter ->
                match parameter with
                | FunctionParameter.PositionalOnly { position; _ }
                | FunctionParameter.Named { position; _ }
                  when position = index ->
                    Some (FunctionParameter.root parameter)
                | _ -> None)
          in
          match parameter with
          | Some parameter ->
              List.filter_map productions ~f:(fun production ->
                  production_to_taint ~root:parameter ~production
                  >>| fun annotation ->
                  ModelParseResult.ModelAnnotation.ParameterAnnotation
                    { root = parameter; annotation; generation_if_source = false })
          | None -> [])
      | ModelQuery.Model.AllParameters { excludes; taint } ->
          let apply_parameter_production (parameter, production) =
            if
              (not (List.is_empty excludes))
              && List.mem
                   excludes
                   ~equal:String.equal
                   (FunctionParameter.name parameter |> Option.value ~default:"")
            then
              None
            else
              let root = FunctionParameter.root parameter in
              production_to_taint ~root ~production
              >>| fun annotation ->
              ModelParseResult.ModelAnnotation.ParameterAnnotation
                { root; annotation; generation_if_source = false }
          in
          List.cartesian_product parameters taint |> List.filter_map ~f:apply_parameter_production
      | ModelQuery.Model.Parameter { where; taint; _ } ->
          let apply_parameter_production (parameter, production) =
            if
              List.for_all
                where
                ~f:
                  (parameter_matches_constraint
                     ~pyre_api
                     ~class_hierarchy_graph
                     ~name_captures
                     ~parameter)
            then
              let root = FunctionParameter.root parameter in
              production_to_taint ~root ~production
              >>| fun annotation ->
              ModelParseResult.ModelAnnotation.ParameterAnnotation
                { root; annotation; generation_if_source = false }
            else
              None
          in
          List.cartesian_product parameters taint |> List.filter_map ~f:apply_parameter_production
      | ModelQuery.Model.Modes modes -> [ModelParseResult.ModelAnnotation.ModeAnnotation modes]
      | ModelQuery.Model.Attribute _ -> failwith "impossible case"
      | ModelQuery.Model.Global _ -> failwith "impossible case"
      | ModelQuery.Model.WriteToCache _ -> failwith "impossible case"
    in
    let captures = Modelable.captures modelable in
    let parameters = Modelable.parameters_of_signatures modelable in
    List.concat_map models ~f:(apply_model ~parameters ~captures)


  let generate_model_from_annotations
      ~pyre_api
      ~source_sink_filter
      ~stubs
      ~target:callable
      ~modelable
      annotations
    =
    ModelParser.create_callable_model_from_annotations
      ~pyre_api
      ~modelable
      ~source_sink_filter
      ~is_obscure:(Interprocedural.Target.HashsetSharedMemory.ReadOnly.mem stubs callable)
      annotations
end)

module AttributeQueryExecutor = struct
  let get_attributes ~scheduler ~pyre_api =
    let () = Log.info "Fetching all attributes..." in
    let get_class_attributes class_name =
      let class_name_reference = Reference.create class_name in
      PyrePysaApi.ReadOnly.get_class_attributes
        pyre_api
        ~include_generated_attributes:false
        ~only_simple_assignments:true
        class_name
      |> Option.value ~default:[]
      |> List.map ~f:(fun attribute_name ->
             Target.create_object (Reference.create ~prefix:class_name_reference attribute_name))
    in
    let all_classes = PyrePysaApi.ReadOnly.all_classes pyre_api ~scheduler in
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    let map = List.concat_map ~f:get_class_attributes in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map
      ~reduce:List.append
      ~inputs:all_classes
      ()


  include MakeQueryExecutor (struct
    type annotation = TaintAnnotation.t

    let query_kind_name = "attribute"

    let schedule_identifier = Configuration.ScheduleIdentifier.AttributeModelQueries

    let make_modelable ~pyre_api ~callables_to_definitions_map:_ target =
      Modelable.create_attribute ~pyre_api target


    let generate_annotations_from_query_models
        ~pyre_api:_
        ~class_hierarchy_graph:_
        ~name_captures:_
        ~modelable:_
        models
      =
      let production_to_taint = function
        | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation -> Some taint_annotation
        | _ -> None
      in
      let apply_model = function
        | ModelQuery.Model.Attribute productions ->
            List.filter_map productions ~f:production_to_taint
        | _ -> failwith "impossible case"
      in
      List.concat_map models ~f:apply_model


    let generate_model_from_annotations
        ~pyre_api
        ~source_sink_filter
        ~stubs:_
        ~target
        ~modelable:_
        annotations
      =
      ModelParser.create_attribute_model_from_annotations
        ~pyre_api
        ~name:(Target.object_name target)
        ~source_sink_filter
        annotations
  end)
end

module GlobalVariableQueryExecutor = struct
  let get_globals ~scheduler ~pyre_api =
    let () = Log.info "Fetching all globals..." in
    let filter_global global_reference =
      match PyrePysaApi.ReadOnly.get_unannotated_global pyre_api global_reference with
      | Some (TupleAssign _)
      | Some (SimpleAssign _) ->
          true
      | _ -> false
    in
    PyrePysaApi.ReadOnly.all_unannotated_globals pyre_api ~scheduler
    |> List.filter ~f:filter_global
    |> List.map ~f:Target.create_object


  include MakeQueryExecutor (struct
    type annotation = TaintAnnotation.t

    let query_kind_name = "global"

    let schedule_identifier = Configuration.ScheduleIdentifier.GlobalModelQueries

    let make_modelable ~pyre_api ~callables_to_definitions_map:_ target =
      Modelable.create_global ~pyre_api target


    (* Generate taint annotations from the `models` part of a given model query. *)
    let generate_annotations_from_query_models
        ~pyre_api:_
        ~class_hierarchy_graph:_
        ~name_captures:_
        ~modelable:_
        models
      =
      let production_to_taint = function
        | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation -> Some taint_annotation
        | _ -> None
      in
      let apply_model = function
        | ModelQuery.Model.Global productions -> List.filter_map productions ~f:production_to_taint
        | _ -> []
      in
      List.concat_map models ~f:apply_model


    let generate_model_from_annotations
        ~pyre_api
        ~source_sink_filter
        ~stubs:_
        ~target
        ~modelable:_
        annotations
      =
      ModelParser.create_attribute_model_from_annotations
        ~pyre_api
        ~name:(Target.object_name target)
        ~source_sink_filter
        annotations
  end)
end

let generate_models_from_queries
    ~pyre_api
    ~scheduler
    ~scheduler_policies
    ~callables_to_definitions_map
    ~class_hierarchy_graph
    ~source_sink_filter
    ~verbose
    ~error_on_unexpected_models
    ~error_on_empty_result
    ~definitions_and_stubs
    ~stubs
    queries
  =
  let queries =
    match source_sink_filter with
    | Some source_sink_filter ->
        List.filter ~f:(ModelParseResult.ModelQuery.should_keep ~source_sink_filter) queries
    | None -> queries
  in
  let extends_to_classnames =
    ModelParseResult.ModelQuery.extract_extends_from_model_queries queries
  in
  let class_hierarchy_graph =
    ClassHierarchyGraph.SharedMemory.from_heap
      ~store_transitive_children_for:extends_to_classnames
      class_hierarchy_graph
  in

  let { PartitionTargetQueries.callable_queries; attribute_queries; global_queries } =
    PartitionTargetQueries.partition queries
  in

  let execution_result = ExecutionResult.create_empty () in

  (* Generate models for functions and methods. *)
  let execution_result =
    if not (List.is_empty callable_queries) then
      CallableQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~pyre_api
        ~scheduler
        ~scheduler_policies
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:definitions_and_stubs
        ~queries:callable_queries
        execution_result
    else
      execution_result
  in

  (* Generate models for attributes. *)
  let execution_result =
    if not (List.is_empty attribute_queries) then
      let attributes = AttributeQueryExecutor.get_attributes ~scheduler ~pyre_api in
      AttributeQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~pyre_api
        ~scheduler
        ~scheduler_policies
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:attributes
        ~queries:attribute_queries
        execution_result
    else
      execution_result
  in

  (* Generate models for globals. *)
  let execution_result =
    if not (List.is_empty global_queries) then
      let globals = GlobalVariableQueryExecutor.get_globals ~scheduler ~pyre_api in
      GlobalVariableQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~pyre_api
        ~scheduler
        ~scheduler_policies
        ~callables_to_definitions_map
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:globals
        ~queries:global_queries
        execution_result
    else
      execution_result
  in

  let execution_result =
    if error_on_unexpected_models then
      ExecutionResult.check_expected_and_unexpected_model_errors ~queries execution_result
    else
      execution_result
  in
  let execution_result =
    if error_on_empty_result then
      ExecutionResult.errors_for_queries_without_output ~queries execution_result
    else
      execution_result
  in
  execution_result
