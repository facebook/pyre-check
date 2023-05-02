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
open Analysis
open Interprocedural
open ModelParseResult

(* Represents the result of generating models from queries. *)
module ModelQueryRegistryMap = struct
  type t = Registry.t String.Map.t

  let empty = String.Map.empty

  let add model_query_map ~model_query_identifier ~registry =
    if not (Registry.is_empty registry) then
      String.Map.update model_query_map model_query_identifier ~f:(function
          | None -> registry
          | Some existing -> Registry.merge_skewed ~join:Model.join_user_models existing registry)
    else
      model_query_map


  let get = String.Map.find

  let merge ~model_join left right =
    String.Map.merge left right ~f:(fun ~key:_ -> function
      | `Both (models1, models2) -> Some (Registry.merge_skewed ~join:model_join models1 models2)
      | `Left models
      | `Right models ->
          Some models)


  let to_alist = String.Map.to_alist ~key_order:`Increasing

  let mapi model_query_map ~f =
    String.Map.mapi
      ~f:(fun ~key ~data -> f ~model_query_identifier:key ~models:data)
      model_query_map


  let get_model_query_identifiers = String.Map.keys

  let get_models = String.Map.data

  let merge_all_registries ~model_join registries =
    Algorithms.fold_balanced registries ~init:Registry.empty ~f:(Registry.merge ~join:model_join)


  let get_registry ~model_join model_query_map =
    merge_all_registries ~model_join (get_models model_query_map)


  let check_expected_and_unexpected_model_errors ~model_query_results ~queries =
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
           ~f:(fun { ModelQuery.name; path; location; expected_models; unexpected_models; _ } ->
             let actual_models =
               Option.value (get model_query_results name) ~default:Registry.empty
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


  let check_errors ~model_query_results ~queries =
    let module LoggingGroup = struct
      type t = {
        name: string;
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
      let model_query_identifier = ModelQuery.unique_identifier query in
      let models_count =
        get model_query_results model_query_identifier
        |> Option.value ~default:Registry.empty
        |> Registry.size
      in
      match logging_group_name with
      | None ->
          let () =
            Statistics.log_model_query_outputs
              ~is_group:false
              ~model_query_name:model_query_identifier
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
            | None -> { LoggingGroup.name = logging_group_name; models_count; location; path }
            | Some existing -> LoggingGroup.add existing models_count
          in
          let logging_group_map =
            String.Map.update ~f:update logging_group_map logging_group_name
          in
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
    let errors = String.Map.fold logging_group_map ~f:check_logging_group ~init:errors in
    Statistics.flush ();
    errors
end

(* Helper functions to dump generated models into a string or file. *)
module DumpModelQueryResults = struct
  let dump_to_string ~model_query_results =
    let model_to_json (callable, model) =
      `Assoc
        [
          "callable", `String (Target.external_name callable);
          ( "model",
            Model.to_json
              ~expand_overrides:None
              ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
              ~filename_lookup:None
              ~export_leaf_names:Domains.ExportLeafNames.Always
              callable
              model );
        ]
    in
    let to_json ~key:model_query_identifier ~data:models =
      models
      |> Registry.to_alist
      |> List.map ~f:model_to_json
      |> fun models ->
      `List models |> fun models_json -> `Assoc [model_query_identifier, models_json]
    in
    `List (String.Map.data (String.Map.mapi model_query_results ~f:to_json))
    |> Yojson.Safe.pretty_to_string


  let dump_to_file ~model_query_results ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    path |> File.create ~content:(dump_to_string ~model_query_results) |> File.write


  let dump_to_file_and_string ~model_query_results ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    let content = dump_to_string ~model_query_results in
    path |> File.create ~content |> File.write;
    content
end

let sanitized_location_insensitive_compare left right =
  let sanitize_decorator_argument ({ Expression.Call.Argument.name; value } as argument) =
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
    { argument with name = new_name; value = new_value }
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
      (match name_captures with
      | Some name_captures when is_match ->
          NameCaptures.add name_captures (Re2.first_match_exn pattern name)
      | _ -> ());
      is_match


let rec matches_decorator_constraint ~name_captures ~decorator = function
  | ModelQuery.DecoratorConstraint.AnyOf constraints ->
      List.exists constraints ~f:(matches_decorator_constraint ~name_captures ~decorator)
  | ModelQuery.DecoratorConstraint.AllOf constraints ->
      List.for_all constraints ~f:(matches_decorator_constraint ~name_captures ~decorator)
  | ModelQuery.DecoratorConstraint.Not decorator_constraint ->
      not (matches_decorator_constraint ~name_captures ~decorator decorator_constraint)
  | ModelQuery.DecoratorConstraint.NameConstraint name_constraint ->
      let { Statement.Decorator.name = { Node.value = decorator_name; _ }; _ } = decorator in
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (decorator_name |> Reference.delocalize |> Reference.last)
  | ModelQuery.DecoratorConstraint.FullyQualifiedNameConstraint name_constraint ->
      let { Statement.Decorator.name = { Node.value = decorator_name; _ }; _ } = decorator in
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (decorator_name |> Reference.delocalize |> Reference.show)
  | ModelQuery.DecoratorConstraint.ArgumentsConstraint arguments_constraint -> (
      let { Statement.Decorator.arguments = decorator_arguments; _ } = decorator in
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
          && SanitizedCallArgumentSet.is_subset
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


let matches_annotation_constraint
    ~resolution
    ~class_hierarchy_graph
    ~name_captures
    ~annotation_constraint
    annotation
  =
  let open Expression in
  match annotation_constraint, annotation with
  | ( ModelQuery.AnnotationConstraint.IsAnnotatedTypeConstraint,
      {
        Node.value =
          Expression.Call
            {
              Call.callee =
                {
                  Node.value =
                    Name
                      (Name.Attribute
                        {
                          base =
                            { Node.value = Name (Name.Attribute { attribute = "Annotated"; _ }); _ };
                          _;
                        });
                  _;
                };
              _;
            };
        _;
      } ) ->
      true
  | ModelQuery.AnnotationConstraint.NameConstraint name_constraint, annotation_expression ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (Expression.show annotation_expression)
  | ( ModelQuery.AnnotationConstraint.AnnotationClassExtends
        { class_name; is_transitive; includes_self },
      annotation_expression ) -> (
      let extract_readonly t =
        match t with
        | Type.ReadOnly t -> t
        | t -> t
      in
      let extract_optional t =
        match Type.optional_value t with
        | Some t -> t
        | _ -> t
      in
      let rec extract_class_name t =
        match Reference.show (Type.class_name t) with
        | "typing.Any"
        | "typing.Union"
        | "typing.Callable"
        | "$bottom"
        | "$unknown" ->
            None
        | "typing.Optional" -> extract_class_name (extract_optional t)
        | "pyre_extensions.ReadOnly" -> extract_class_name (extract_readonly t)
        | extracted_class_name -> Some extracted_class_name
      in
      let parsed_type = GlobalResolution.parse_annotation resolution annotation_expression in
      match extract_class_name parsed_type with
      | Some extracted_class_name ->
          find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name
          |> ClassHierarchyGraph.ClassNameSet.mem extracted_class_name
      | None -> false)
  | _ -> false


let rec normalized_parameter_matches_constraint
    ~resolution
    ~class_hierarchy_graph
    ~name_captures
    ~parameter:
      ((root, parameter_name, { Node.value = { Expression.Parameter.annotation; _ }; _ }) as
      parameter)
  = function
  | ModelQuery.ParameterConstraint.AnnotationConstraint annotation_constraint ->
      annotation
      >>| matches_annotation_constraint
            ~resolution
            ~class_hierarchy_graph
            ~name_captures
            ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.ParameterConstraint.NameConstraint name_constraint ->
      matches_name_constraint ~name_captures ~name_constraint (Identifier.sanitized parameter_name)
  | ModelQuery.ParameterConstraint.IndexConstraint index -> (
      match root with
      | AccessPath.Root.PositionalParameter { position; _ } when position = index -> true
      | _ -> false)
  | ModelQuery.ParameterConstraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:
          (normalized_parameter_matches_constraint
             ~resolution
             ~class_hierarchy_graph
             ~name_captures
             ~parameter)
  | ModelQuery.ParameterConstraint.Not query_constraint ->
      not
        (normalized_parameter_matches_constraint
           ~resolution
           ~class_hierarchy_graph
           ~name_captures
           ~parameter
           query_constraint)
  | ModelQuery.ParameterConstraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:
          (normalized_parameter_matches_constraint
             ~resolution
             ~class_hierarchy_graph
             ~name_captures
             ~parameter)


let class_matches_decorator_constraint ~name_captures ~resolution ~decorator_constraint class_name =
  GlobalResolution.class_summary resolution (Type.Primitive class_name)
  >>| Node.value
  >>| (fun { decorators; _ } ->
        List.exists decorators ~f:(fun decorator ->
            Statement.Decorator.from_expression decorator
            >>| (fun decorator ->
                  matches_decorator_constraint ~name_captures ~decorator decorator_constraint)
            |> Option.value ~default:false))
  |> Option.value ~default:false


let find_parents ~resolution ~is_transitive ~includes_self class_name =
  let parents =
    if is_transitive then
      Analysis.ClassHierarchy.successors (GlobalResolution.class_hierarchy resolution) class_name
    else
      Analysis.ClassHierarchy.immediate_parents
        (GlobalResolution.class_hierarchy resolution)
        class_name
  in
  let parents =
    if includes_self then
      class_name :: parents
    else
      parents
  in
  parents


let rec class_matches_constraint ~resolution ~class_hierarchy_graph ~name_captures ~name = function
  | ModelQuery.ClassConstraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:(class_matches_constraint ~resolution ~class_hierarchy_graph ~name_captures ~name)
  | ModelQuery.ClassConstraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:(class_matches_constraint ~resolution ~class_hierarchy_graph ~name_captures ~name)
  | ModelQuery.ClassConstraint.Not class_constraint ->
      not
        (class_matches_constraint
           ~resolution
           ~name
           ~class_hierarchy_graph
           ~name_captures
           class_constraint)
  | ModelQuery.ClassConstraint.NameConstraint name_constraint ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (name |> Reference.create |> Reference.last)
  | ModelQuery.ClassConstraint.FullyQualifiedNameConstraint name_constraint ->
      matches_name_constraint ~name_captures ~name_constraint name
  | ModelQuery.ClassConstraint.Extends { class_name; is_transitive; includes_self } ->
      find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name
      |> ClassHierarchyGraph.ClassNameSet.mem name
  | ModelQuery.ClassConstraint.DecoratorConstraint decorator_constraint ->
      class_matches_decorator_constraint ~name_captures ~resolution ~decorator_constraint name
  | ModelQuery.ClassConstraint.AnyChildConstraint { class_constraint; is_transitive; includes_self }
    ->
      find_children ~class_hierarchy_graph ~is_transitive ~includes_self name
      |> ClassHierarchyGraph.ClassNameSet.exists (fun name ->
             class_matches_constraint
               ~resolution
               ~name
               ~class_hierarchy_graph
               ~name_captures
               class_constraint)
  | ModelQuery.ClassConstraint.AnyParentConstraint
      { class_constraint; is_transitive; includes_self } ->
      find_parents ~resolution ~is_transitive ~includes_self name
      |> List.exists ~f:(fun name ->
             class_matches_constraint
               ~resolution
               ~name
               ~class_hierarchy_graph
               ~name_captures
               class_constraint)


let rec matches_constraint ~resolution ~class_hierarchy_graph ~name_captures value query_constraint =
  match query_constraint with
  | ModelQuery.Constraint.AnyOf constraints ->
      List.exists
        constraints
        ~f:(matches_constraint ~resolution ~class_hierarchy_graph ~name_captures value)
  | ModelQuery.Constraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:(matches_constraint ~resolution ~class_hierarchy_graph ~name_captures value)
  | ModelQuery.Constraint.Not query_constraint ->
      not
        (matches_constraint
           ~resolution
           ~class_hierarchy_graph
           ~name_captures
           value
           query_constraint)
  | ModelQuery.Constraint.NameConstraint name_constraint ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (value |> Modelable.name |> Reference.last)
  | ModelQuery.Constraint.FullyQualifiedNameConstraint name_constraint ->
      matches_name_constraint
        ~name_captures
        ~name_constraint
        (value |> Modelable.name |> Reference.show)
  | ModelQuery.Constraint.AnnotationConstraint annotation_constraint ->
      Modelable.type_annotation value
      >>| matches_annotation_constraint
            ~resolution
            ~class_hierarchy_graph
            ~name_captures
            ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.Constraint.ReturnConstraint annotation_constraint ->
      Modelable.return_annotation value
      >>| matches_annotation_constraint
            ~resolution
            ~class_hierarchy_graph
            ~name_captures
            ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.Constraint.AnyParameterConstraint parameter_constraint ->
      Modelable.parameters value
      |> AccessPath.Root.normalize_parameters
      |> List.exists ~f:(fun parameter ->
             normalized_parameter_matches_constraint
               ~resolution
               ~class_hierarchy_graph
               ~name_captures
               ~parameter
               parameter_constraint)
  | ModelQuery.Constraint.ReadFromCache _ ->
      (* This is handled before matching constraints. *)
      true
  | ModelQuery.Constraint.AnyDecoratorConstraint decorator_constraint ->
      Modelable.decorators value
      |> List.exists ~f:(fun decorator ->
             Statement.Decorator.from_expression decorator
             >>| (fun decorator ->
                   matches_decorator_constraint ~name_captures ~decorator decorator_constraint)
             |> Option.value ~default:false)
  | ModelQuery.Constraint.ClassConstraint class_constraint ->
      Modelable.class_name value
      >>| (fun name ->
            class_matches_constraint
              ~resolution
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
    | ModelQuery.Constraint.Not _
    | ModelQuery.Constraint.NameConstraint _
    | ModelQuery.Constraint.FullyQualifiedNameConstraint _
    | ModelQuery.Constraint.AnnotationConstraint _
    | ModelQuery.Constraint.ReturnConstraint _
    | ModelQuery.Constraint.AnyParameterConstraint _
    | ModelQuery.Constraint.AnyDecoratorConstraint _
    | ModelQuery.Constraint.ClassConstraint _ ->
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

  val make_modelable : resolution:GlobalResolution.t -> Target.t -> Modelable.t

  (* Generate taint annotations from the `models` part of a given model query. *)
  val generate_annotations_from_query_models
    :  resolution:GlobalResolution.t ->
    class_hierarchy_graph:ClassHierarchyGraph.SharedMemory.t ->
    modelable:Modelable.t ->
    ModelQuery.Model.t list ->
    annotation list

  val generate_model_from_annotations
    :  resolution:GlobalResolution.t ->
    source_sink_filter:SourceSinkFilter.t option ->
    stubs:Target.t Hash_set.t ->
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
      ~resolution
      ~class_hierarchy_graph
      ~name_captures
      ~modelable
      { ModelQuery.find; where; name = query_name; _ }
    =
    let result =
      Modelable.matches_find modelable find
      && List.for_all
           ~f:(matches_constraint ~resolution ~class_hierarchy_graph ~name_captures modelable)
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
      ~resolution
      ~class_hierarchy_graph
      ~modelable
      ({ ModelQuery.models; _ } as query)
    =
    if
      matches_query_constraints
        ~verbose
        ~resolution
        ~class_hierarchy_graph
        ~name_captures:None
        ~modelable
        query
    then
      QueryKind.generate_annotations_from_query_models
        ~resolution
        ~class_hierarchy_graph
        ~modelable
        models
    else
      []


  let generate_model_from_query_on_target
      ~verbose
      ~resolution
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
        ~resolution
        ~class_hierarchy_graph
        ~modelable
        query
    with
    | [] -> Ok None
    | annotations ->
        QueryKind.generate_model_from_annotations
          ~resolution
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          annotations
        |> Result.map ~f:Option.some


  let generate_models_from_query_on_targets
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      query
    =
    let fold registry target =
      let modelable = QueryKind.make_modelable ~resolution target in
      match
        generate_model_from_query_on_target
          ~verbose
          ~resolution
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          query
      with
      | Ok (Some model) -> Registry.add registry ~join:Model.join_user_models ~target ~model
      | Ok None -> registry
      | Error error ->
          let () =
            Log.error "Error while executing model query: %s" (ModelVerificationError.display error)
          in
          registry
    in
    List.fold targets ~init:Registry.empty ~f:fold


  let generate_models_from_queries_on_target
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~queries
      target
    =
    let modelable = QueryKind.make_modelable ~resolution target in
    let fold model_query_results query =
      let identifier = ModelQuery.unique_identifier query in
      match
        generate_model_from_query_on_target
          ~verbose
          ~resolution
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~target
          ~modelable
          query
      with
      | Ok (Some model) ->
          let registry = Registry.singleton ~target ~model in
          ModelQueryRegistryMap.add model_query_results ~model_query_identifier:identifier ~registry
      | Ok None -> model_query_results
      | Error error ->
          let () =
            Log.error "Error while executing model query: %s" (ModelVerificationError.display error)
          in
          model_query_results
    in
    List.fold queries ~init:ModelQueryRegistryMap.empty ~f:fold


  let generate_models_from_queries_on_targets
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      queries
    =
    let fold model_query_results target =
      let model_query =
        generate_models_from_queries_on_target
          ~verbose
          ~resolution
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~queries
          target
      in
      ModelQueryRegistryMap.merge ~model_join:Model.join_user_models model_query_results model_query
    in
    List.fold targets ~init:ModelQueryRegistryMap.empty ~f:fold


  let generate_cache_from_query_on_target
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~initial_cache
      ~target
      ({ ModelQuery.models; name; _ } as query)
    =
    let name_captures = NameCaptures.create () in
    let modelable = QueryKind.make_modelable ~resolution target in
    let write_to_cache cache = function
      | ModelQuery.Model.WriteToCache { kind; name } ->
          ReadWriteCache.write
            cache
            ~kind
            ~name:(Modelable.expand_write_to_cache ~name_captures modelable name)
            ~target
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
        ~resolution
        ~class_hierarchy_graph
        ~name_captures:(Some name_captures)
        ~modelable
        query
    then
      List.fold ~init:initial_cache ~f:write_to_cache models
    else
      initial_cache


  let generate_cache_from_queries_on_targets
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~targets
      write_to_cache_queries
    =
    let fold_target ~query cache target =
      generate_cache_from_query_on_target
        ~verbose
        ~resolution
        ~class_hierarchy_graph
        ~initial_cache:cache
        ~target
        query
    in
    let fold_query cache query = List.fold targets ~init:cache ~f:(fold_target ~query) in
    List.fold write_to_cache_queries ~init:ReadWriteCache.empty ~f:fold_query


  let generate_cache_from_queries_on_targets_with_multiprocessing
      ~verbose
      ~resolution
      ~scheduler
      ~class_hierarchy_graph
      ~targets
    = function
    | [] -> ReadWriteCache.empty
    | write_to_cache_queries ->
        let map cache targets =
          generate_cache_from_queries_on_targets
            ~verbose
            ~resolution
            ~class_hierarchy_graph
            ~targets
            write_to_cache_queries
          |> ReadWriteCache.merge cache
        in
        Scheduler.map_reduce
          scheduler
          ~policy:
            (Scheduler.Policy.fixed_chunk_count
               ~minimum_chunks_per_worker:1
               ~minimum_chunk_size:1000
               ~preferred_chunks_per_worker:1
               ())
          ~initial:ReadWriteCache.empty
          ~map
          ~reduce:ReadWriteCache.merge
          ~inputs:targets
          ()


  let generate_models_from_read_cache_queries_on_targets
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~cache
      read_from_cache_queries
    =
    let fold model_query_results ({ ModelQuery.name = model_query_name; where; _ } as query) =
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
          let identifier = ModelQuery.unique_identifier query in
          let registry =
            generate_models_from_query_on_targets
              ~verbose
              ~resolution
              ~class_hierarchy_graph
              ~source_sink_filter
              ~stubs
              ~targets:(Target.Set.elements candidates)
              query
          in
          ModelQueryRegistryMap.add model_query_results ~model_query_identifier:identifier ~registry
    in
    List.fold read_from_cache_queries ~init:ModelQueryRegistryMap.empty ~f:fold


  (* Generate models from non-cache queries. *)
  let generate_models_from_regular_queries_on_targets_with_multiprocessing
      ~verbose
      ~resolution
      ~scheduler
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
    = function
    | [] -> ModelQueryRegistryMap.empty
    | regular_queries ->
        let map model_query_results targets =
          generate_models_from_queries_on_targets
            ~verbose
            ~resolution
            ~class_hierarchy_graph
            ~source_sink_filter
            ~stubs
            ~targets
            regular_queries
          |> ModelQueryRegistryMap.merge ~model_join:Model.join_user_models model_query_results
        in
        let reduce = ModelQueryRegistryMap.merge ~model_join:Model.join_user_models in
        Scheduler.map_reduce
          scheduler
          ~policy:
            (Scheduler.Policy.fixed_chunk_count
               ~minimum_chunks_per_worker:1
               ~minimum_chunk_size:1000
               ~preferred_chunks_per_worker:1
               ())
          ~initial:ModelQueryRegistryMap.empty
          ~map
          ~reduce
          ~inputs:targets
          ()


  let generate_models_from_queries_on_targets_with_multiprocessing
      ~verbose
      ~resolution
      ~scheduler
      ~class_hierarchy_graph
      ~source_sink_filter
      ~stubs
      ~targets
      queries
    =
    let {
      PartitionCacheQueries.write_to_cache = write_to_cache_queries;
      read_from_cache = read_from_cache_queries;
      others = regular_queries;
    }
      =
      PartitionCacheQueries.partition queries
    in

    let model_query_results_cache_queries =
      let () =
        Log.info
          "Building cache for %d %s model queries..."
          (List.length write_to_cache_queries)
          QueryKind.query_kind_name
      in
      let cache =
        generate_cache_from_queries_on_targets_with_multiprocessing
          ~verbose
          ~resolution
          ~scheduler
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
      let results =
        generate_models_from_read_cache_queries_on_targets
          ~verbose
          ~resolution
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~cache
          read_from_cache_queries
      in
      let () =
        Log.info
          "Generated %d models from cached %s model queries."
          (List.length (ModelQueryRegistryMap.get_models results))
          QueryKind.query_kind_name
      in
      results
    in

    let model_query_results_regular_queries =
      let () =
        Log.info
          "Generating models from %d regular %s model queries..."
          (List.length regular_queries)
          QueryKind.query_kind_name
      in
      let results =
        generate_models_from_regular_queries_on_targets_with_multiprocessing
          ~verbose
          ~resolution
          ~scheduler
          ~class_hierarchy_graph
          ~source_sink_filter
          ~stubs
          ~targets
          regular_queries
      in
      let () =
        Log.info
          "Generated %d models from regular %s model queries."
          (List.length (ModelQueryRegistryMap.get_models results))
          QueryKind.query_kind_name
      in
      results
    in

    ModelQueryRegistryMap.merge
      ~model_join:Model.join_user_models
      model_query_results_regular_queries
      model_query_results_cache_queries
end

module CallableQueryExecutor = MakeQueryExecutor (struct
  type annotation = ModelAnnotation.t

  let query_kind_name = "callable"

  let make_modelable ~resolution callable =
    let signature =
      lazy
        (match Target.get_module_and_definition ~resolution callable with
        | Some (_, { Node.value = { signature; _ }; _ }) -> signature
        | None ->
            (* This should only be called with valid targets, generated from `FetchCallables`. *)
            Format.asprintf
              "unknown target `%a` in `CallableQueryExecutor`"
              Target.pp_external
              callable
            |> failwith)
    in
    Modelable.Callable { target = callable; signature }


  let generate_annotations_from_query_models ~resolution ~class_hierarchy_graph ~modelable models =
    let production_to_taint ?(parameter = None) ~production annotation =
      let open Expression in
      let get_subkind_from_annotation ~pattern annotation =
        let get_annotation_of_type annotation =
          match annotation >>| Node.value with
          | Some (Expression.Call { Call.callee = { Node.value = callee; _ }; arguments }) -> (
              match callee with
              | Name
                  (Name.Attribute
                    {
                      base =
                        { Node.value = Name (Name.Attribute { attribute = "Annotated"; _ }); _ };
                      _;
                    }) -> (
                  match arguments with
                  | [
                   { Call.Argument.value = { Node.value = Expression.Tuple [_; annotation]; _ }; _ };
                  ] ->
                      Some annotation
                  | _ -> None)
              | _ -> None)
          | _ -> None
        in
        match get_annotation_of_type annotation with
        | Some
            {
              Node.value =
                Expression.Call
                  {
                    Call.callee = { Node.value = Name (Name.Identifier callee_name); _ };
                    arguments =
                      [
                        {
                          Call.Argument.value = { Node.value = Name (Name.Identifier subkind); _ };
                          _;
                        };
                      ];
                  };
              _;
            } ->
            if String.equal callee_name pattern then
              Some subkind
            else
              None
        | _ -> None
      in
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
        match parameter, taint_annotation with
        | Some actual_parameter, ModelParseResult.TaintAnnotation.Source { source; features } ->
            let via_features =
              List.map ~f:(update_placeholder_via_feature ~actual_parameter) features.via_features
            in
            ModelParseResult.TaintAnnotation.Source
              { source; features = { features with via_features } }
        | Some actual_parameter, ModelParseResult.TaintAnnotation.Sink { sink; features } ->
            let via_features =
              List.map ~f:(update_placeholder_via_feature ~actual_parameter) features.via_features
            in
            ModelParseResult.TaintAnnotation.Sink
              { sink; features = { features with via_features } }
        | Some actual_parameter, ModelParseResult.TaintAnnotation.Tito { tito; features } ->
            let via_features =
              List.map ~f:(update_placeholder_via_feature ~actual_parameter) features.via_features
            in
            ModelParseResult.TaintAnnotation.Tito
              { tito; features = { features with via_features } }
        | Some actual_parameter, ModelParseResult.TaintAnnotation.AddFeatureToArgument { features }
          ->
            let via_features =
              List.map ~f:(update_placeholder_via_feature ~actual_parameter) features.via_features
            in
            ModelParseResult.TaintAnnotation.AddFeatureToArgument
              { features = { features with via_features } }
        | _ -> taint_annotation
      in
      match production with
      | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation ->
          Some (update_placeholder_via_features taint_annotation)
      | ModelQuery.QueryTaintAnnotation.ParametricSourceFromAnnotation { source_pattern; kind } ->
          get_subkind_from_annotation ~pattern:source_pattern annotation
          >>| fun subkind ->
          ModelParseResult.TaintAnnotation.Source
            {
              source = Sources.ParametricSource { source_name = kind; subkind };
              features = ModelParseResult.TaintFeatures.empty;
            }
      | ModelQuery.QueryTaintAnnotation.ParametricSinkFromAnnotation { sink_pattern; kind } ->
          get_subkind_from_annotation ~pattern:sink_pattern annotation
          >>| fun subkind ->
          ModelParseResult.TaintAnnotation.Sink
            {
              sink = Sinks.ParametricSink { sink_name = kind; subkind };
              features = ModelParseResult.TaintFeatures.empty;
            }
    in
    let apply_model ~normalized_parameters ~return_annotation = function
      | ModelQuery.Model.Return productions ->
          List.filter_map productions ~f:(fun production ->
              production_to_taint return_annotation ~production
              >>| fun taint -> ModelParseResult.ModelAnnotation.ReturnAnnotation taint)
      | ModelQuery.Model.NamedParameter { name; taint = productions } -> (
          let parameter =
            List.find_map
              normalized_parameters
              ~f:(fun
                   (root, parameter_name, { Node.value = { Expression.Parameter.annotation; _ }; _ })
                 ->
                if Identifier.equal_sanitized parameter_name name then
                  Some (root, annotation)
                else
                  None)
          in
          match parameter with
          | Some (parameter, annotation) ->
              List.filter_map productions ~f:(fun production ->
                  production_to_taint annotation ~production
                  >>| fun taint ->
                  ModelParseResult.ModelAnnotation.ParameterAnnotation (parameter, taint))
          | None -> [])
      | ModelQuery.Model.PositionalParameter { index; taint = productions } -> (
          let parameter =
            List.find_map
              normalized_parameters
              ~f:(fun (root, _, { Node.value = { Expression.Parameter.annotation; _ }; _ }) ->
                match root with
                | AccessPath.Root.PositionalParameter { position; _ } when position = index ->
                    Some (root, annotation)
                | _ -> None)
          in
          match parameter with
          | Some (parameter, annotation) ->
              List.filter_map productions ~f:(fun production ->
                  production_to_taint annotation ~production
                  >>| fun taint ->
                  ModelParseResult.ModelAnnotation.ParameterAnnotation (parameter, taint))
          | None -> [])
      | ModelQuery.Model.AllParameters { excludes; taint } ->
          let apply_parameter_production
              ( (root, parameter_name, { Node.value = { Expression.Parameter.annotation; _ }; _ }),
                production )
            =
            if
              (not (List.is_empty excludes))
              && List.mem excludes ~equal:String.equal (Identifier.sanitized parameter_name)
            then
              None
            else
              production_to_taint annotation ~production
              >>| fun taint -> ModelParseResult.ModelAnnotation.ParameterAnnotation (root, taint)
          in
          List.cartesian_product normalized_parameters taint
          |> List.filter_map ~f:apply_parameter_production
      | ModelQuery.Model.Parameter { where; taint; _ } ->
          let apply_parameter_production
              ( ((root, _, { Node.value = { Expression.Parameter.annotation; _ }; _ }) as parameter),
                production )
            =
            if
              List.for_all
                where
                ~f:
                  (normalized_parameter_matches_constraint
                     ~resolution
                     ~class_hierarchy_graph
                     ~name_captures:None
                     ~parameter)
            then
              let parameter, _, _ = parameter in
              production_to_taint annotation ~production ~parameter:(Some parameter)
              >>| fun taint -> ModelParseResult.ModelAnnotation.ParameterAnnotation (root, taint)
            else
              None
          in
          List.cartesian_product normalized_parameters taint
          |> List.filter_map ~f:apply_parameter_production
      | ModelQuery.Model.Modes modes -> [ModelParseResult.ModelAnnotation.ModeAnnotation modes]
      | ModelQuery.Model.Attribute _ -> failwith "impossible case"
      | ModelQuery.Model.Global _ -> failwith "impossible case"
      | ModelQuery.Model.WriteToCache _ -> failwith "impossible case"
    in
    let { Statement.Define.Signature.parameters; return_annotation; _ } =
      match modelable with
      | Modelable.Callable { signature; _ } -> Lazy.force signature
      | _ -> failwith "unreachable"
    in
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    List.concat_map models ~f:(apply_model ~normalized_parameters ~return_annotation)


  let generate_model_from_annotations
      ~resolution
      ~source_sink_filter
      ~stubs
      ~target:callable
      ~modelable
      annotations
    =
    ModelParser.create_callable_model_from_annotations
      ~resolution
      ~modelable
      ~source_sink_filter
      ~is_obscure:(Hash_set.mem stubs callable)
      annotations
end)

module AttributeQueryExecutor = struct
  let get_attributes ~resolution =
    let () = Log.info "Fetching all attributes..." in
    let get_class_attributes class_name =
      let class_summary =
        GlobalResolution.class_summary resolution (Type.Primitive class_name) >>| Node.value
      in
      match class_summary with
      | None -> []
      | Some ({ name = class_name_reference; _ } as class_summary) ->
          let attributes, constructor_attributes =
            ( ClassSummary.attributes ~include_generated_attributes:false class_summary,
              ClassSummary.constructor_attributes class_summary )
          in
          let all_attributes =
            Identifier.SerializableMap.union (fun _ x _ -> Some x) attributes constructor_attributes
          in
          let get_target_from_attributes attribute_name attribute accumulator =
            match Node.value attribute with
            | { ClassSummary.Attribute.kind = Simple _; _ } ->
                Target.create_object (Reference.create ~prefix:class_name_reference attribute_name)
                :: accumulator
            | _ -> accumulator
          in
          Identifier.SerializableMap.fold get_target_from_attributes all_attributes []
    in
    let all_classes =
      resolution
      |> GlobalResolution.unannotated_global_environment
      |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
    in
    List.concat_map all_classes ~f:get_class_attributes


  let get_type_annotation ~resolution class_name attribute =
    let get_annotation = function
      | { ClassSummary.Attribute.kind = Simple { ClassSummary.Attribute.annotation; _ }; _ } ->
          annotation
      | _ -> None
    in
    GlobalResolution.class_summary resolution (Type.Primitive class_name)
    >>| Node.value
    >>= fun class_summary ->
    match
      ClassSummary.constructor_attributes class_summary
      |> Identifier.SerializableMap.find_opt attribute
      >>| Node.value
      >>| get_annotation
    with
    | Some annotation -> annotation
    | None ->
        ClassSummary.attributes ~include_generated_attributes:false class_summary
        |> Identifier.SerializableMap.find_opt attribute
        >>| Node.value
        >>= get_annotation


  include MakeQueryExecutor (struct
    type annotation = TaintAnnotation.t

    let query_kind_name = "attribute"

    let make_modelable ~resolution target =
      let name = Target.object_name target in
      let type_annotation =
        lazy
          (let class_name = Reference.prefix name >>| Reference.show |> Option.value ~default:"" in
           let attribute = Reference.last name in
           get_type_annotation ~resolution class_name attribute)
      in
      Modelable.Attribute { name; type_annotation }


    let generate_annotations_from_query_models
        ~resolution:_
        ~class_hierarchy_graph:_
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
        ~resolution
        ~source_sink_filter
        ~stubs:_
        ~target
        ~modelable:_
        annotations
      =
      ModelParser.create_attribute_model_from_annotations
        ~resolution
        ~name:(Target.object_name target)
        ~source_sink_filter
        annotations
  end)
end

module GlobalVariableQueryExecutor = struct
  let get_globals ~resolution =
    let () = Log.info "Fetching all globals..." in
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment resolution
    in
    let filter_global global_reference =
      match
        UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
          unannotated_global_environment
          global_reference
      with
      | Some (TupleAssign _)
      | Some (SimpleAssign _) ->
          true
      | _ -> false
    in
    unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals
    |> List.filter ~f:filter_global
    |> List.map ~f:Target.create_object


  let get_type_annotation ~resolution reference =
    match
      UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
        (GlobalResolution.unannotated_global_environment resolution)
        reference
    with
    | Some (SimpleAssign { explicit_annotation; _ }) -> explicit_annotation
    | _ -> None


  include MakeQueryExecutor (struct
    type annotation = TaintAnnotation.t

    let query_kind_name = "global"

    let make_modelable ~resolution target =
      let name = Target.object_name target in
      let type_annotation = lazy (get_type_annotation ~resolution name) in
      Modelable.Global { name; type_annotation }


    (* Generate taint annotations from the `models` part of a given model query. *)
    let generate_annotations_from_query_models
        ~resolution:_
        ~class_hierarchy_graph:_
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
        ~resolution
        ~source_sink_filter
        ~stubs:_
        ~target
        ~modelable:_
        annotations
      =
      ModelParser.create_attribute_model_from_annotations
        ~resolution
        ~name:(Target.object_name target)
        ~source_sink_filter
        annotations
  end)
end

let generate_models_from_queries
    ~resolution
    ~scheduler
    ~class_hierarchy_graph
    ~source_sink_filter
    ~verbose
    ~callables_and_stubs
    ~stubs
    queries
  =
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

  (* Generate models for functions and methods. *)
  let model_query_results =
    if not (List.is_empty callable_queries) then
      CallableQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~resolution
        ~scheduler
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:callables_and_stubs
        callable_queries
    else
      ModelQueryRegistryMap.empty
  in

  (* Generate models for attributes. *)
  let model_query_results =
    if not (List.is_empty attribute_queries) then
      let attributes = AttributeQueryExecutor.get_attributes ~resolution in
      AttributeQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~resolution
        ~scheduler
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:attributes
        attribute_queries
      |> ModelQueryRegistryMap.merge ~model_join:Model.join_user_models model_query_results
    else
      model_query_results
  in

  (* Generate models for globals. *)
  let model_query_results =
    if not (List.is_empty global_queries) then
      let globals = GlobalVariableQueryExecutor.get_globals ~resolution in
      GlobalVariableQueryExecutor.generate_models_from_queries_on_targets_with_multiprocessing
        ~verbose
        ~resolution
        ~scheduler
        ~class_hierarchy_graph
        ~source_sink_filter
        ~stubs
        ~targets:globals
        global_queries
      |> ModelQueryRegistryMap.merge ~model_join:Model.join_user_models model_query_results
    else
      model_query_results
  in

  let errors =
    List.rev_append
      (ModelQueryRegistryMap.check_expected_and_unexpected_model_errors
         ~model_query_results
         ~queries)
      (ModelQueryRegistryMap.check_errors ~model_query_results ~queries)
  in

  model_query_results, errors
