(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PyrePysaApi is a wrapper around a type checker API, which exposes source code, ASTs and type
   information about the code to analyze. Right now, this wraps either the old Pyre 1 API provided
   by `Analysis.PyrePysaEnvironment` or the Pyrefly API provided by `Interprocedural.Pyrefly`. *)

module Pyre1Api = Analysis.PyrePysaEnvironment
module ScalarTypeProperties = Pyre1Api.ScalarTypeProperties
module ClassNamesFromType = Pyre1Api.ClassNamesFromType
module PysaType = Pyre1Api.PysaType
module PyreClassSummary = Pyre1Api.PyreClassSummary
module AstResult = Pyre1Api.AstResult

module PysaClassSummary = struct
  type t =
    | Pyre1 of Pyre1Api.PysaClassSummary.t
    | Pyrefly of PyreflyApi.PysaClassSummary.t

  let pyre1_find_attribute = function
    | Pyre1 class_summary -> Pyre1Api.PysaClassSummary.find_attribute class_summary
    | Pyrefly _ -> failwith "unimplemented: PysaClassSummary.pyre1_find_attribute"


  let pyre1_get_attributes = function
    | Pyre1 class_summary -> Pyre1Api.PysaClassSummary.get_attributes class_summary
    | Pyrefly _ -> failwith "unimplemented: PysaClassSummary.pyre1_get_attributes"
end

module ReadWrite = struct
  type t =
    | Pyre1 of Pyre1Api.ReadWrite.t
    | Pyrefly of PyreflyApi.ReadWrite.t

  let load_from_cache ~configuration ~pyrefly_results =
    match pyrefly_results with
    | Some _ -> failwith "unimplemented: ReadWrite.load_from_cache"
    | None -> Pyre1 (Pyre1Api.ReadWrite.load_from_cache ~configuration)


  let create_with_cold_start
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~pyrefly_results
      ~decorator_configuration
      ~skip_type_checking_callables
      ~callback_with_qualifiers_and_definitions
    =
    match pyrefly_results with
    | Some pyrefly_results ->
        Pyrefly
          (PyreflyApi.ReadWrite.create_from_directory
             ~scheduler
             ~scheduler_policies
             ~configuration
             pyrefly_results)
    | None ->
        Pyre1
          (Pyre1Api.ReadWrite.create_with_cold_start
             ~scheduler
             ~scheduler_policies
             ~configuration
             ~decorator_configuration
             ~skip_type_checking_callables
             ~callback_with_qualifiers_and_definitions)


  let configuration = function
    | Pyre1 pyre_api -> Pyre1Api.ReadWrite.configuration pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadWrite.configuration"


  (* Only used for caching *)
  let module_paths = function
    | Pyre1 pyre_api -> Pyre1Api.ReadWrite.module_paths pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadWrite.module_paths"


  (* Only used for caching *)
  let module_paths_from_disk = function
    | Pyre1 pyre_api -> Pyre1Api.ReadWrite.module_paths_from_disk pyre_api
    | Pyrefly _ -> failwith "unimplemented"


  let save = function
    | Pyre1 pyre_api -> Pyre1Api.ReadWrite.save pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadWrite.save"


  let purge_sources_from_shared_memory = function
    | Pyre1 pyre_api -> Pyre1Api.ReadWrite.purge_sources_from_shared_memory pyre_api
    | Pyrefly _ -> () (* Nothing to do, whole source AST are not stored in shared memory. *)


  let parse_type_of_expressions api ~scheduler ~scheduler_policies =
    match api with
    | Pyre1 _ -> api (* Nothing to do, types are inferred when requested. *)
    | Pyrefly pyrefly_api ->
        Pyrefly
          (PyreflyApi.ReadWrite.parse_type_of_expressions
             pyrefly_api
             ~scheduler
             ~scheduler_policies)
end

module ReadOnly = struct
  type t =
    | Pyre1 of Pyre1Api.ReadOnly.t
    | Pyrefly of PyreflyApi.ReadOnly.t

  let of_read_write_api = function
    | ReadWrite.Pyre1 pyre_api -> Pyre1 (Pyre1Api.ReadOnly.of_read_write_api pyre_api)
    | Pyrefly pyrefly_api -> Pyrefly (PyreflyApi.ReadOnly.of_read_write_api pyrefly_api)


  let from_pyre1_environment ~type_environment ~global_module_paths_api =
    Pyre1 (Pyre1Api.ReadOnly.create ~type_environment ~global_module_paths_api)


  let from_pyre1_api pyre_api = Pyre1 pyre_api

  let from_pyrefly_api pyrefly_api = Pyrefly pyrefly_api

  let is_pyrefly = function
    | Pyre1 _ -> false
    | Pyrefly _ -> true


  let explicit_qualifiers = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.explicit_qualifiers pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.explicit_qualifiers pyrefly_api


  let absolute_source_path_of_qualifier ~lookup_source = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.absolute_source_path_of_qualifier ~lookup_source pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.absolute_source_path_of_qualifier pyrefly_api


  let relative_path_of_qualifier api qualifier =
    match api with
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.relative_path_of_qualifier pyre_api qualifier
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.relative_path_of_qualifier"


  let source_of_qualifier = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.source_of_qualifier pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.source_of_qualifier"


  let get_class_names_for_qualifier = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_class_names_for_qualifier pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_names_for_qualifier pyrefly_api


  let module_exists = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.module_exists pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.module_exists"


  let parse_annotation = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.parse_annotation pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.parse_annotation"


  let get_class_summary api class_name =
    let open Core.Option.Monad_infix in
    match api with
    | Pyre1 pyre_api ->
        Pyre1Api.ReadOnly.get_class_summary pyre_api class_name
        >>| fun class_summary -> PysaClassSummary.Pyre1 class_summary
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_class_summary pyrefly_api class_name
        |> fun class_summary -> Some (PysaClassSummary.Pyrefly class_summary)


  let get_class_decorators_opt = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_class_decorators_opt pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_decorators_opt pyrefly_api


  let get_class_attributes = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_class_attributes pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_class_attributes pyrefly_api


  let source_is_unit_test = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.source_is_unit_test pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.source_is_unit_test"


  let class_immediate_parents = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.class_immediate_parents pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.class_immediate_parents pyrefly_api


  let class_mro = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.class_mro pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.class_mro pyrefly_api


  let get_define_names_for_qualifier api ~exclude_test_modules qualifier =
    match api with
    | Pyre1 _ when exclude_test_modules ->
        failwith
          "exclude_test_modules=true is not supported for \
           PyrePysaEnvironment.ReadOnly.get_define_names_for_qualifier"
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_define_names_for_qualifier pyre_api qualifier
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_define_names_for_qualifier
          pyrefly_api
          ~exclude_test_modules
          qualifier


  let get_methods_for_qualifier api ~exclude_test_modules qualifier =
    match api with
    | Pyre1 pyre_api ->
        Pyre1Api.ReadOnly.get_methods_for_qualifier ~exclude_test_modules pyre_api qualifier
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_methods_for_qualifier ~exclude_test_modules pyrefly_api qualifier


  let get_qualifier_top_level_define_name = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_qualifier_top_level_define_name pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_qualifier_top_level_define_name pyrefly_api


  let parse_reference = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.parse_reference pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.parse_reference"


  let class_exists = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.class_exists pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.class_exists"


  let get_callable_captures = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_callable_captures pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_captures pyrefly_api


  let get_callable_return_annotations = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_callable_return_annotations pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_return_annotations pyrefly_api


  let get_callable_parameter_annotations = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_callable_parameter_annotations pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.get_callable_parameter_annotations pyrefly_api


  let get_variable = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_variable pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_variable"


  let resolve_define = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_define pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_define"


  let resolve_define_undecorated = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_define_undecorated pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_define_undecorated"


  let global = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.global pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.global"


  let get_overriden_base_method api ~class_name ~method_name =
    match api with
    | Pyre1 pyre_api ->
        Pyre1Api.ReadOnly.get_overriden_base_method pyre_api ~class_name ~method_name
    | Pyrefly pyrefly_api ->
        PyreflyApi.ReadOnly.get_overriden_base_method pyrefly_api ~class_name ~method_name


  let annotation_parser = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.annotation_parser pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.annotation_parser"


  let less_or_equal = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.less_or_equal pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.less_or_equal"


  let resolve_exports = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_exports pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_exports"


  let location_of_global = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.location_of_global pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.location_of_global"


  let get_function_definition = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_function_definition pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_function_definition"


  let attribute_from_class_name = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.attribute_from_class_name pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.attribute_from_class_name"


  let has_transitive_successor = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.has_transitive_successor pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.has_transitive_successor"


  let has_transitive_successor_ignoring_untracked = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.has_transitive_successor_ignoring_untracked pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.has_transitive_successor_ignoring_untracked"


  let exists_matching_class_decorator = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.exists_matching_class_decorator pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.exists_matching_class_decorator"


  let generic_parameters_as_variables = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.generic_parameters_as_variables pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.generic_parameters_as_variables"


  let decorated_define = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.decorated_define pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.decorated_define"


  (* TODO(T225700656): Move this in the ClassSummary module *)
  let named_tuple_attributes api class_name =
    match api with
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.named_tuple_attributes pyre_api class_name
    | Pyrefly pyrefly_api ->
        let class_summary = PyreflyApi.ReadOnly.get_class_summary pyrefly_api class_name in
        if PyreflyApi.ReadOnly.ClassSummary.is_named_tuple pyrefly_api class_summary then
          Some (PyreflyApi.ReadOnly.ClassSummary.named_tuple_attributes pyrefly_api class_summary)
        else
          None


  let resolve_expression_to_type_info = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_expression_to_type_info pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_expression_to_type_info"


  let get_unannotated_global = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_unannotated_global pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_unannotated_global"


  let all_classes = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.all_classes pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.all_classes pyrefly_api


  let all_unannotated_globals = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.all_unannotated_globals pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.all_unannotated_globals"


  let ensures_qualified api source =
    match api with
    | Pyre1 _ -> source
    | Pyrefly _ -> Preprocessing.qualify source


  module Type = struct
    let scalar_properties = function
      | Pyre1 pyre_api -> Pyre1Api.ReadOnly.Type.scalar_properties pyre_api
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.scalar_properties pyrefly_api


    let get_class_names = function
      | Pyre1 pyre_api -> Pyre1Api.ReadOnly.Type.get_class_names pyre_api
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.get_class_names pyrefly_api


    let is_dictionary_or_mapping = function
      | Pyre1 pyre_api -> Pyre1Api.ReadOnly.Type.is_dictionary_or_mapping pyre_api
      | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.Type.is_dictionary_or_mapping pyrefly_api
  end

  module ClassSummary = struct
    let has_custom_new api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.has_custom_new pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.has_custom_new pyrefly_api class_summary
      | _ -> failwith "unexpected"


    let is_dataclass api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.is_dataclass pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_dataclass pyrefly_api class_summary
      | _ -> failwith "unexpected"


    let is_named_tuple api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.is_named_tuple pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_named_tuple pyrefly_api class_summary
      | _ -> failwith "unexpected"


    let is_typed_dict api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.is_typed_dict pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.is_typed_dict pyrefly_api class_summary
      | _ -> failwith "unexpected"


    let dataclass_ordered_attributes api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.dataclass_ordered_attributes pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.dataclass_ordered_attributes pyrefly_api class_summary
      | _ -> failwith "unexpected"


    let typed_dictionary_attributes api class_summary =
      match api, class_summary with
      | Pyre1 pyre_api, PysaClassSummary.Pyre1 class_summary ->
          Pyre1Api.ReadOnly.ClassSummary.typed_dictionary_attributes pyre_api class_summary
      | Pyrefly pyrefly_api, PysaClassSummary.Pyrefly class_summary ->
          PyreflyApi.ReadOnly.ClassSummary.typed_dictionary_attributes pyrefly_api class_summary
      | _ -> failwith "unexpected"
  end

  let add_builtins_prefix api reference =
    match api with
    | Pyre1 _ -> reference
    | Pyrefly _ -> PyreflyApi.add_builtins_prefix reference


  (* Given a fully qualified name for a function, method, class, attribute or global variable,
     return its 'symbolic' name. This removes any path prefix and suffixes such as `@setter` and
     `$2`. *)
  let target_symbolic_name api reference =
    match api with
    | Pyre1 _ -> reference
    | Pyrefly _ -> PyreflyApi.target_symbolic_name reference
end

module InContext = struct
  type t =
    | Pyre1 of Pyre1Api.InContext.t
    | Pyrefly of PyreflyApi.InContext.t

  let create_at_global_scope = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1 (Pyre1Api.InContext.create_at_global_scope pyre_api)
    | ReadOnly.Pyrefly pyrefly_api ->
        Pyrefly (PyreflyApi.InContext.create_at_global_scope pyrefly_api)


  let create_at_statement_key api ~define_name ~define ~statement_key =
    match api with
    | ReadOnly.Pyre1 pyre_api ->
        Pyre1
          (Pyre1Api.InContext.create_at_statement_key pyre_api ~define_name ~define ~statement_key)
    | ReadOnly.Pyrefly pyrefly_api ->
        Pyrefly
          (PyreflyApi.InContext.create_at_statement_key pyrefly_api ~define_name ~statement_key)


  let pyre_api = function
    | Pyre1 pyre_context -> ReadOnly.Pyre1 (Pyre1Api.InContext.pyre_api pyre_context)
    | Pyrefly pyrefly_context -> ReadOnly.Pyrefly (PyreflyApi.InContext.pyre_api pyrefly_context)


  let is_global = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.is_global pyre_context
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.is_global pyrefly_context


  let resolve_reference = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_reference pyre_context
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_reference pyrefly_context


  let resolve_assignment api assign =
    match api with
    | Pyre1 pyre_context -> Pyre1 (Pyre1Api.InContext.resolve_assignment pyre_context assign)
    | Pyrefly pyrefly_context ->
        Pyrefly (PyreflyApi.InContext.resolve_assignment pyrefly_context assign)


  let resolve_expression_to_type = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_expression_to_type pyre_context
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_expression_to_type pyrefly_context


  let resolve_attribute_access = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_attribute_access pyre_context
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.resolve_attribute_access pyrefly_context


  let fallback_attribute = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.fallback_attribute pyre_context
    | Pyrefly pyrefly_context -> PyreflyApi.InContext.fallback_attribute pyrefly_context


  let resolve_generators api generators =
    match api with
    | Pyre1 pyre_context -> Pyre1 (Pyre1Api.InContext.resolve_generators pyre_context generators)
    | Pyrefly pyrefly_context ->
        Pyrefly (PyreflyApi.InContext.resolve_generators pyrefly_context generators)
end

module ModelQueries = struct
  let property_decorators = Pyre1Api.ModelQueries.property_decorators

  module FunctionParameter = Pyre1Api.ModelQueries.FunctionParameter
  module FunctionParameters = Pyre1Api.ModelQueries.FunctionParameters
  module FunctionSignature = Analysis.PyrePysaEnvironment.ModelQueries.FunctionSignature
  module Function = Pyre1Api.ModelQueries.Function
  module Global = Pyre1Api.ModelQueries.Global

  let resolve_qualified_name_to_global = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1Api.ModelQueries.resolve_qualified_name_to_global pyre_api
    | ReadOnly.Pyrefly pyrefly_api ->
        PyreflyApi.ModelQueries.resolve_qualified_name_to_global pyrefly_api


  let class_method_signatures = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1Api.ModelQueries.class_method_signatures pyre_api
    | ReadOnly.Pyrefly pyrefly_api -> PyreflyApi.ModelQueries.class_method_signatures pyrefly_api


  let invalidate_cache = function
    | ReadOnly.Pyre1 _ -> Pyre1Api.ModelQueries.invalidate_cache ()
    | ReadOnly.Pyrefly _ -> ()
end
