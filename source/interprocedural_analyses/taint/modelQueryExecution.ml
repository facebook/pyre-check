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
open Pyre
open Ast
open Analysis
open Interprocedural
open ModelParseResult

module VariableMetadata = struct
  type t = {
    name: Ast.Reference.t;
    type_annotation: Ast.Expression.Expression.t option;
  }
  [@@deriving show, compare]
end

module ModelQueryRegistryMap = struct
  type t = Registry.t String.Map.t

  let empty = String.Map.empty

  let set model_query_map ~model_query_name ~models =
    String.Map.set ~key:model_query_name ~data:models model_query_map


  let get = String.Map.find

  let merge ~model_join left right =
    String.Map.merge left right ~f:(fun ~key:_ -> function
      | `Both (models1, models2) -> Some (Registry.merge ~join:model_join models1 models2)
      | `Left models
      | `Right models ->
          Some models)


  let to_alist = String.Map.to_alist ~key_order:`Increasing

  let mapi model_query_map ~f =
    String.Map.mapi ~f:(fun ~key ~data -> f ~model_query_name:key ~models:data) model_query_map


  let get_model_query_names = String.Map.keys

  let get_models = String.Map.data

  let merge_all_registries ~model_join registries =
    List.fold registries ~init:Registry.empty ~f:(Registry.merge ~join:model_join)


  let get_registry ~model_join model_query_map =
    merge_all_registries ~model_join (get_models model_query_map)


  let check_expected_and_unexpected_model_errors ~models_and_names ~queries =
    let find_expected_and_unexpected_model_errors ~expect ~actual_models ~name ~location ~models =
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
          [{ ModelVerificationError.kind; location; path = None }]
    in
    let find_expected_model_errors ~actual_models ~name ~location ~expected_models =
      find_expected_and_unexpected_model_errors
        ~expect:true
        ~actual_models
        ~name
        ~location
        ~models:expected_models
    in
    let find_unexpected_model_errors ~actual_models ~name ~location ~unexpected_models =
      find_expected_and_unexpected_model_errors
        ~expect:false
        ~actual_models
        ~name
        ~location
        ~models:unexpected_models
    in
    let expected_and_unexpected_model_errors =
      queries
      |> List.map ~f:(fun { ModelQuery.name; location; expected_models; unexpected_models; _ } ->
             let actual_models = Option.value (get models_and_names name) ~default:Registry.empty in
             let expected_model_errors =
               match expected_models with
               | [] -> []
               | _ -> find_expected_model_errors ~actual_models ~name ~location ~expected_models
             in
             let unexpected_model_errors =
               match unexpected_models with
               | [] -> []
               | _ -> find_unexpected_model_errors ~actual_models ~name ~location ~unexpected_models
             in
             List.append expected_model_errors unexpected_model_errors)
      |> List.concat
    in
    expected_and_unexpected_model_errors


  let check_errors ~models_and_names ~queries =
    let model_query_names = List.map queries ~f:(fun query -> query.ModelQuery.name) in
    let errors =
      List.filter_map model_query_names ~f:(fun model_query_name ->
          let models = get models_and_names model_query_name in
          Statistics.log_model_query_outputs
            ~model_query_name
            ~generated_models_count:(Registry.size (Option.value models ~default:Registry.empty))
            ();
          match models with
          | Some _ -> None
          | None ->
              Some
                {
                  ModelVerificationError.kind =
                    ModelVerificationError.NoOutputFromModelQuery model_query_name;
                  location = Ast.Location.any;
                  path = None;
                })
    in
    Statistics.flush ();
    errors
end

module DumpModelQueryResults : sig
  val dump_to_string : models_and_names:ModelQueryRegistryMap.t -> string

  val dump_to_file : models_and_names:ModelQueryRegistryMap.t -> path:PyrePath.t -> unit

  val dump_to_file_and_string
    :  models_and_names:ModelQueryRegistryMap.t ->
    path:PyrePath.t ->
    string
end = struct
  let dump_to_string ~models_and_names =
    let model_to_json (callable, model) =
      `Assoc
        [
          "callable", `String (Target.external_name callable);
          ( "model",
            Model.to_json
              ~expand_overrides:None
              ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
              ~filename_lookup:None
              callable
              model );
        ]
    in
    let to_json ~key:model_query_name ~data:models =
      models
      |> Registry.to_alist
      |> List.map ~f:model_to_json
      |> fun models ->
      `List models
      |> fun models_json ->
      `Assoc [(* TODO(T123305362) also include filenames *) model_query_name, models_json]
    in
    `List (String.Map.data (String.Map.mapi models_and_names ~f:to_json))
    |> Yojson.Safe.pretty_to_string


  let dump_to_file ~models_and_names ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    path |> File.create ~content:(dump_to_string ~models_and_names) |> File.write


  let dump_to_file_and_string ~models_and_names ~path =
    Log.warning "Emitting the model query results to `%s`" (PyrePath.absolute path);
    let content = dump_to_string ~models_and_names in
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

let is_ancestor ~resolution ~is_transitive ~includes_self ancestor_class child_class =
  if String.equal ancestor_class child_class then
    includes_self
  else if is_transitive then
    try
      GlobalResolution.is_transitive_successor
        ~placeholder_subclass_extends_all:false
        resolution
        ~predecessor:child_class
        ~successor:ancestor_class
    with
    | ClassHierarchy.Untracked _ -> false
  else
    let parents = GlobalResolution.immediate_parents ~resolution child_class in
    List.mem parents ancestor_class ~equal:String.equal


let matches_name_constraint ~name_constraint =
  match name_constraint with
  | ModelQuery.NameConstraint.Equals string -> String.equal string
  | ModelQuery.NameConstraint.Matches pattern -> Re2.matches pattern


let matches_decorator_constraint ~name_constraint ~arguments_constraint decorator =
  let decorator_name_matches { Statement.Decorator.name = { Node.value = decorator_name; _ }; _ } =
    matches_name_constraint ~name_constraint (Reference.show decorator_name)
  in
  let decorator_arguments_matches { Statement.Decorator.arguments = decorator_arguments; _ } =
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
    | None, _ -> true
    | Some (ModelQuery.ArgumentsConstraint.Contains constraint_arguments), None ->
        List.is_empty constraint_arguments
    | Some (ModelQuery.ArgumentsConstraint.Contains constraint_arguments), Some arguments ->
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
    | Some (ModelQuery.ArgumentsConstraint.Equals constraint_arguments), None ->
        List.is_empty constraint_arguments
    | Some (ModelQuery.ArgumentsConstraint.Equals constraint_arguments), Some arguments ->
        let constraint_positional_arguments, constraint_keyword_arguments =
          split_arguments constraint_arguments
        in
        let decorator_positional_arguments, decorator_keyword_arguments =
          split_arguments arguments
        in
        (* Since equality comparison is more costly, check the lists are the same lengths first. *)
        Int.equal
          (List.length constraint_positional_arguments)
          (List.length decorator_positional_arguments)
        && positional_arguments_equal constraint_positional_arguments decorator_positional_arguments
        && SanitizedCallArgumentSet.equal
             (SanitizedCallArgumentSet.of_list constraint_keyword_arguments)
             (SanitizedCallArgumentSet.of_list decorator_keyword_arguments)
  in
  match Statement.Decorator.from_expression decorator with
  | None -> false
  | Some decorator -> decorator_name_matches decorator && decorator_arguments_matches decorator


let matches_annotation_constraint ~annotation_constraint annotation =
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
      matches_name_constraint ~name_constraint (Expression.show annotation_expression)
  | _ -> false


let rec normalized_parameter_matches_constraint
    ~resolution
    ~parameter:
      ((root, parameter_name, { Node.value = { Expression.Parameter.annotation; _ }; _ }) as
      parameter)
  = function
  | ModelQuery.ParameterConstraint.AnnotationConstraint annotation_constraint ->
      annotation
      >>| matches_annotation_constraint ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.ParameterConstraint.NameConstraint name_constraint ->
      matches_name_constraint ~name_constraint (Identifier.sanitized parameter_name)
  | ModelQuery.ParameterConstraint.IndexConstraint index -> (
      match root with
      | AccessPath.Root.PositionalParameter { position; _ } when position = index -> true
      | _ -> false)
  | ModelQuery.ParameterConstraint.AnyOf constraints ->
      List.exists constraints ~f:(normalized_parameter_matches_constraint ~resolution ~parameter)
  | ModelQuery.ParameterConstraint.Not query_constraint ->
      not (normalized_parameter_matches_constraint ~resolution ~parameter query_constraint)
  | ModelQuery.ParameterConstraint.AllOf constraints ->
      List.for_all constraints ~f:(normalized_parameter_matches_constraint ~resolution ~parameter)


let class_matches_decorator_constraint ~resolution ~name_constraint ~arguments_constraint class_name
  =
  GlobalResolution.class_summary resolution (Type.Primitive class_name)
  >>| Node.value
  >>| (fun { decorators; _ } ->
        List.exists decorators ~f:(fun decorator ->
            matches_decorator_constraint ~name_constraint ~arguments_constraint decorator))
  |> Option.value ~default:false


let rec find_children ~class_hierarchy_graph ~is_transitive ~includes_self class_name =
  let child_name_set = ClassHierarchyGraph.SharedMemory.get ~class_name class_hierarchy_graph in
  let child_name_set =
    if is_transitive then
      ClassHierarchyGraph.ClassNameSet.fold
        (fun child_name set ->
          ClassHierarchyGraph.ClassNameSet.union
            set
            (find_children ~class_hierarchy_graph ~is_transitive ~includes_self:false child_name))
        child_name_set
        child_name_set
    else
      child_name_set
  in
  let child_name_set =
    if includes_self then
      ClassHierarchyGraph.ClassNameSet.add class_name child_name_set
    else
      child_name_set
  in
  child_name_set


let rec class_matches_constraint ~resolution ~class_hierarchy_graph ~name class_constraint =
  match class_constraint with
  | ModelQuery.ClassConstraint.AnyOf constraints ->
      List.exists constraints ~f:(class_matches_constraint ~resolution ~class_hierarchy_graph ~name)
  | ModelQuery.ClassConstraint.AllOf constraints ->
      List.for_all
        constraints
        ~f:(class_matches_constraint ~resolution ~class_hierarchy_graph ~name)
  | ModelQuery.ClassConstraint.Not class_constraint ->
      not (class_matches_constraint ~resolution ~name ~class_hierarchy_graph class_constraint)
  | ModelQuery.ClassConstraint.NameConstraint name_constraint ->
      matches_name_constraint ~name_constraint name
  | ModelQuery.ClassConstraint.Extends { class_name; is_transitive; includes_self } ->
      is_ancestor ~resolution ~is_transitive ~includes_self class_name name
  | ModelQuery.ClassConstraint.DecoratorConstraint { name_constraint; arguments_constraint } ->
      class_matches_decorator_constraint ~resolution ~name_constraint ~arguments_constraint name
  | ModelQuery.ClassConstraint.AnyChildConstraint { class_constraint; is_transitive; includes_self }
    ->
      find_children ~class_hierarchy_graph ~is_transitive ~includes_self name
      |> ClassHierarchyGraph.ClassNameSet.exists (fun name ->
             class_matches_constraint ~resolution ~name ~class_hierarchy_graph class_constraint)


module Modelable = struct
  type t =
    | Callable of {
        target: Target.t;
        definition: Statement.Define.t Node.t option Lazy.t;
      }
    | Attribute of VariableMetadata.t
    | Global of VariableMetadata.t

  let name = function
    | Callable { target; _ } -> Target.external_name target
    | Attribute { VariableMetadata.name; _ }
    | Global { VariableMetadata.name; _ } ->
        Reference.show name


  let type_annotation = function
    | Callable _ -> failwith "unexpected use of type_annotation on a callable"
    | Attribute { VariableMetadata.type_annotation; _ }
    | Global { VariableMetadata.type_annotation; _ } ->
        type_annotation


  let return_annotation = function
    | Callable { definition; _ } -> (
        match Lazy.force definition with
        | Some
            {
              Node.value =
                {
                  Statement.Define.signature = { Statement.Define.Signature.return_annotation; _ };
                  _;
                };
              _;
            } ->
            return_annotation
        | _ -> None)
    | Attribute _
    | Global _ ->
        failwith "unexpected use of return_annotation on an attribute or global"


  let parameters = function
    | Callable { definition; _ } -> (
        match Lazy.force definition with
        | Some
            {
              Node.value =
                { Statement.Define.signature = { Statement.Define.Signature.parameters; _ }; _ };
              _;
            } ->
            Some parameters
        | _ -> None)
    | Attribute _
    | Global _ ->
        failwith "unexpected use of any_parameter on an attribute or global"


  let decorators = function
    | Callable { definition; _ } -> (
        match Lazy.force definition with
        | Some
            {
              Node.value =
                { Statement.Define.signature = { Statement.Define.Signature.decorators; _ }; _ };
              _;
            } ->
            Some decorators
        | _ -> None)
    | Attribute _
    | Global _ ->
        failwith "unexpected use of Decorator on an attribute or global"


  let class_name = function
    | Callable { target; _ } -> Target.class_name target
    | Attribute { VariableMetadata.name; _ } -> Reference.prefix name >>| Reference.show
    | Global _ -> failwith "unexpected use of a class constraint on a global"
end

let rec matches_constraint ~resolution ~class_hierarchy_graph value query_constraint =
  match query_constraint with
  | ModelQuery.Constraint.AnyOf constraints ->
      List.exists constraints ~f:(matches_constraint ~resolution ~class_hierarchy_graph value)
  | ModelQuery.Constraint.AllOf constraints ->
      List.for_all constraints ~f:(matches_constraint ~resolution ~class_hierarchy_graph value)
  | ModelQuery.Constraint.Not query_constraint ->
      not (matches_constraint ~resolution ~class_hierarchy_graph value query_constraint)
  | ModelQuery.Constraint.NameConstraint name_constraint ->
      matches_name_constraint ~name_constraint (Modelable.name value)
  | ModelQuery.Constraint.AnnotationConstraint annotation_constraint ->
      Modelable.type_annotation value
      >>| matches_annotation_constraint ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.Constraint.ReturnConstraint annotation_constraint ->
      Modelable.return_annotation value
      >>| matches_annotation_constraint ~annotation_constraint
      |> Option.value ~default:false
  | ModelQuery.Constraint.AnyParameterConstraint parameter_constraint ->
      Modelable.parameters value
      >>| AccessPath.Root.normalize_parameters
      >>| List.exists ~f:(fun parameter ->
              normalized_parameter_matches_constraint ~resolution ~parameter parameter_constraint)
      |> Option.value ~default:false
  | ModelQuery.Constraint.AnyDecoratorConstraint { name_constraint; arguments_constraint } ->
      Modelable.decorators value
      >>| List.exists ~f:(matches_decorator_constraint ~name_constraint ~arguments_constraint)
      |> Option.value ~default:false
  | ModelQuery.Constraint.ClassConstraint class_constraint ->
      Modelable.class_name value
      >>| (fun name ->
            class_matches_constraint ~resolution ~class_hierarchy_graph ~name class_constraint)
      |> Option.value ~default:false


let apply_callable_models
    ~resolution
    ~models
    {
      Node.value =
        {
          Statement.Define.signature =
            { Statement.Define.Signature.parameters; return_annotation; _ };
          _;
        };
      _;
    }
  =
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
                    base = { Node.value = Name (Name.Attribute { attribute = "Annotated"; _ }); _ };
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
                      { Call.Argument.value = { Node.value = Name (Name.Identifier subkind); _ }; _ };
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
          ModelParseResult.TaintAnnotation.Sink { sink; features = { features with via_features } }
      | Some actual_parameter, ModelParseResult.TaintAnnotation.Tito { tito; features } ->
          let via_features =
            List.map ~f:(update_placeholder_via_feature ~actual_parameter) features.via_features
          in
          ModelParseResult.TaintAnnotation.Tito { tito; features = { features with via_features } }
      | Some actual_parameter, ModelParseResult.TaintAnnotation.AddFeatureToArgument { features } ->
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
  let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
  let apply_model = function
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
          if List.for_all where ~f:(normalized_parameter_matches_constraint ~resolution ~parameter)
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
  in
  List.concat_map models ~f:apply_model


let apply_callable_query
    ~verbose
    ~resolution
    ~class_hierarchy_graph
    ~callable
    { ModelQuery.find; where; models; name = query_name; _ }
  =
  let kind_matches =
    match callable, find with
    | Target.Function _, ModelQuery.Find.Function
    | Target.Method _, ModelQuery.Find.Method ->
        true
    | _ -> false
  in
  let definition =
    lazy
      (let definition = Target.get_module_and_definition ~resolution callable >>| snd in
       let () =
         if Option.is_none definition then
           Log.error "Could not find definition for callable: `%a`" Target.pp_pretty callable
       in
       definition)
  in
  if
    kind_matches
    && List.for_all
         ~f:
           (matches_constraint
              ~resolution
              ~class_hierarchy_graph
              (Modelable.Callable { target = callable; definition }))
         where
  then
    let () =
      if verbose then
        Log.info
          "Target `%a` matches all constraints for the model query `%s`."
          Target.pp_pretty
          callable
          query_name
    in
    let annotations =
      Lazy.force definition
      >>| apply_callable_models ~resolution ~models
      |> Option.value ~default:[]
    in
    String.Map.set String.Map.empty ~key:query_name ~data:annotations
  else
    String.Map.empty


let apply_attribute_models ~models =
  let production_to_taint = function
    | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation -> Some taint_annotation
    | _ -> None
  in
  let apply_model = function
    | ModelQuery.Model.Attribute productions -> List.filter_map productions ~f:production_to_taint
    | _ -> failwith "impossible case"
  in
  List.concat_map models ~f:apply_model


let apply_attribute_query
    ~verbose
    ~resolution
    ~class_hierarchy_graph
    ~variable_metadata:({ VariableMetadata.name; _ } as variable_metadata)
    { ModelQuery.find; where; models; name = query_name; _ }
  =
  if
    ModelQuery.Find.is_attribute find
    && List.for_all
         ~f:
           (matches_constraint
              ~resolution
              ~class_hierarchy_graph
              (Modelable.Attribute variable_metadata))
         where
  then
    let () =
      if verbose then
        Log.info
          "Attribute `%s` matches all constraints for the model query `%s`."
          (Reference.show name)
          query_name
    in
    String.Map.set String.Map.empty ~key:query_name ~data:(apply_attribute_models ~models)
  else
    String.Map.empty


let get_class_attributes ~global_resolution ~class_name =
  let class_summary =
    GlobalResolution.class_summary global_resolution (Type.Primitive class_name) >>| Node.value
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
      let get_name_and_annotation_from_attributes attribute_name attribute accumulator =
        match attribute with
        | {
         Node.value =
           {
             ClassSummary.Attribute.kind =
               ClassSummary.Attribute.Simple { ClassSummary.Attribute.annotation; _ };
             _;
           };
         _;
        } ->
            {
              VariableMetadata.name = Reference.create ~prefix:class_name_reference attribute_name;
              type_annotation = annotation;
            }
            :: accumulator
        | _ -> accumulator
      in
      Identifier.SerializableMap.fold get_name_and_annotation_from_attributes all_attributes []


module GlobalVariableQueries = struct
  let get_globals_and_annotations ~environment =
    let global_resolution = environment |> TypeEnvironment.ReadOnly.global_resolution in
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    let variable_metadata_for_global global_reference =
      match
        UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
          unannotated_global_environment
          global_reference
      with
      | Some (SimpleAssign { explicit_annotation = Some _ as explicit_annotation; _ }) ->
          Some { VariableMetadata.name = global_reference; type_annotation = explicit_annotation }
      | Some (TupleAssign _)
      | Some (SimpleAssign _) ->
          Some { VariableMetadata.name = global_reference; type_annotation = None }
      | _ -> None
    in
    UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals unannotated_global_environment
    |> List.filter_map ~f:variable_metadata_for_global


  let apply_global_models ~models =
    let production_to_taint = function
      | ModelQuery.QueryTaintAnnotation.TaintAnnotation taint_annotation -> Some taint_annotation
      | _ -> None
    in
    let apply_model = function
      | ModelQuery.Model.Global productions -> List.filter_map productions ~f:production_to_taint
      | _ -> []
    in
    List.concat_map models ~f:apply_model


  let apply_global_query
      ~verbose
      ~resolution
      ~class_hierarchy_graph
      ~variable_metadata:({ VariableMetadata.name; _ } as variable_metadata)
      { ModelQuery.find; where; models; name = query_name; _ }
    =
    if
      ModelQuery.Find.is_global find
      && List.for_all
           ~f:
             (matches_constraint
                ~resolution
                ~class_hierarchy_graph
                (Modelable.Global variable_metadata))
           where
    then
      let () =
        if verbose then
          Log.info
            "Global `%s` matches all constraints for the model query `%s`."
            (Reference.show name)
            query_name
      in
      String.Map.set String.Map.empty ~key:query_name ~data:(apply_global_models ~models)
    else
      String.Map.empty
end

let apply_all_queries
    ~resolution
    ~scheduler
    ~taint_configuration
    ~class_hierarchy_graph
    ~source_sink_filter
    ~queries
    ~callables
    ~stubs
    ~environment
  =
  let global_resolution = Resolution.global_resolution resolution in
  if List.length queries > 0 then
    let attribute_queries, global_queries, callable_queries =
      List.partition3_map
        ~f:(fun query ->
          match query.ModelQuery.find with
          | ModelQuery.Find.Attribute -> `Fst query
          | ModelQuery.Find.Global -> `Snd query
          | _ -> `Trd query)
        queries
    in
    let apply_queries models_and_names target ~queries ~apply_query ~model_from_annotation =
      let taint_to_model_and_names =
        queries
        |> List.map ~f:apply_query
        |> List.reduce ~f:(fun left right ->
               String.Map.merge left right ~f:(fun ~key:_ -> function
                 | `Both (taint_annotations_left, taint_annotations_right) ->
                     Some (taint_annotations_left @ taint_annotations_right)
                 | `Left taint_annotations
                 | `Right taint_annotations ->
                     Some taint_annotations))
        |> Option.value ~default:String.Map.empty
      in
      String.Map.map taint_to_model_and_names ~f:(fun taint_to_model ->
          match model_from_annotation ~taint_to_model with
          | Ok model -> Registry.add Registry.empty ~join:Model.join_user_models ~target ~model
          | Error error ->
              Log.error
                "Error while executing model query: %s"
                (ModelVerificationError.display error);
              Registry.empty)
      |> ModelQueryRegistryMap.merge ~model_join:Model.join_user_models models_and_names
    in
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    let verbose = Option.is_some taint_configuration.dump_model_query_results_path in

    (* Generate models for functions and methods. *)
    let apply_queries_for_callable models_and_names callable =
      let callable_model_from_annotation ~taint_to_model =
        ModelParser.create_callable_model_from_annotations
          ~resolution
          ~callable
          ~source_sink_filter
          ~is_obscure:(Hash_set.mem stubs callable)
          taint_to_model
      in
      apply_queries
        ~queries:callable_queries
        ~apply_query:
          (apply_callable_query
             ~verbose
             ~resolution:global_resolution
             ~class_hierarchy_graph
             ~callable)
        ~model_from_annotation:callable_model_from_annotation
        models_and_names
        callable
    in
    let callables =
      List.filter_map callables ~f:(function
          | Target.Function _ as callable -> Some callable
          | Target.Method _ as callable -> Some callable
          | _ -> None)
    in
    let callable_models =
      Scheduler.map_reduce
        scheduler
        ~policy:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunk_size:500
             ~preferred_chunks_per_worker:1
             ())
        ~initial:ModelQueryRegistryMap.empty
        ~map:(fun models_and_names callables ->
          List.fold callables ~init:models_and_names ~f:apply_queries_for_callable)
        ~reduce:(ModelQueryRegistryMap.merge ~model_join:Model.join_user_models)
        ~inputs:callables
        ()
    in
    (* Generate models for attributes. *)
    let apply_queries_for_attribute
        models_and_names
        ({ VariableMetadata.name; _ } as variable_metadata)
      =
      let attribute_model_from_annotation ~taint_to_model =
        ModelParser.create_attribute_model_from_annotations
          ~resolution
          ~name
          ~source_sink_filter
          taint_to_model
      in
      let attribute = Target.create_object name in
      apply_queries
        ~queries:attribute_queries
        ~apply_query:
          (apply_attribute_query
             ~verbose
             ~resolution:global_resolution
             ~class_hierarchy_graph
             ~variable_metadata)
        ~model_from_annotation:attribute_model_from_annotation
        models_and_names
        attribute
    in
    let attribute_models =
      if not (List.is_empty attribute_queries) then
        let all_classes =
          TypeEnvironment.ReadOnly.global_resolution environment
          |> GlobalResolution.unannotated_global_environment
          |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
        in
        let attributes =
          List.concat_map all_classes ~f:(fun class_name ->
              get_class_attributes ~global_resolution ~class_name)
        in
        Scheduler.map_reduce
          scheduler
          ~policy:
            (Scheduler.Policy.fixed_chunk_count
               ~minimum_chunk_size:500
               ~preferred_chunks_per_worker:1
               ())
          ~initial:ModelQueryRegistryMap.empty
          ~map:(fun models_and_names attributes ->
            List.fold attributes ~init:models_and_names ~f:apply_queries_for_attribute)
          ~reduce:(ModelQueryRegistryMap.merge ~model_join:Model.join_user_models)
          ~inputs:attributes
          ()
      else
        ModelQueryRegistryMap.empty
    in
    (* Generate models for globals. *)
    let apply_queries_for_globals
        models_and_names
        ({ VariableMetadata.name; _ } as variable_metadata)
      =
      let global_model_from_annotation ~taint_to_model =
        ModelParser.create_attribute_model_from_annotations
          ~resolution
          ~name
          ~source_sink_filter
          taint_to_model
      in
      let global = Target.create_object name in
      apply_queries
        ~queries:global_queries
        ~apply_query:
          (GlobalVariableQueries.apply_global_query
             ~verbose
             ~resolution:global_resolution
             ~class_hierarchy_graph
             ~variable_metadata)
        ~model_from_annotation:global_model_from_annotation
        models_and_names
        global
    in
    let global_models =
      Scheduler.map_reduce
        scheduler
        ~policy:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunk_size:500
             ~preferred_chunks_per_worker:1
             ())
        ~initial:ModelQueryRegistryMap.empty
        ~map:(fun models_and_names globals ->
          List.fold globals ~init:models_and_names ~f:apply_queries_for_globals)
        ~reduce:(ModelQueryRegistryMap.merge ~model_join:Model.join_user_models)
        ~inputs:(GlobalVariableQueries.get_globals_and_annotations ~environment)
        ()
    in
    let models_and_names =
      ModelQueryRegistryMap.merge
        ~model_join:Model.join_user_models
        callable_models
        attribute_models
      |> ModelQueryRegistryMap.merge ~model_join:Model.join_user_models global_models
    in
    ( models_and_names,
      ModelQueryRegistryMap.check_expected_and_unexpected_model_errors ~models_and_names ~queries
      @ ModelQueryRegistryMap.check_errors ~models_and_names ~queries )
  else
    ModelQueryRegistryMap.empty, []


let generate_models_from_queries
    ~taint_configuration
    ~class_hierarchy_graph
    ~scheduler
    ~environment
    ~source_sink_filter
    ~callables
    ~stubs
    queries
  =
  let resolution =
    Analysis.TypeCheck.resolution
      (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module Analysis.TypeCheck.DummyContext)
  in
  let callables =
    Hash_set.fold stubs ~f:(Core.Fn.flip List.cons) ~init:callables
    |> List.filter_map ~f:(function
           | Target.Function _ as callable -> Some callable
           | Target.Method _ as callable -> Some callable
           | _ -> None)
  in
  apply_all_queries
    ~resolution
    ~scheduler
    ~taint_configuration
    ~class_hierarchy_graph
    ~source_sink_filter
    ~queries
    ~callables
    ~stubs
    ~environment
