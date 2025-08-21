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
             ~decorator_configuration
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
    | Pyrefly _ -> failwith "unimplemented: ReadWrite.purge_sources_from_shared_memory"
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

  let explicit_qualifiers = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.explicit_qualifiers pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.explicit_qualifiers pyrefly_api


  let absolute_source_path_of_qualifier ~lookup_source = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.absolute_source_path_of_qualifier ~lookup_source pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.absolute_source_path_of_qualifier pyrefly_api


  let relative_path_of_qualifier = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.relative_path_of_qualifier pyre_api
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


  let get_class_summary = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_class_summary pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_class_summary"


  let source_is_unit_test = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.source_is_unit_test pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.source_is_unit_test"


  let class_immediate_parents = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.class_immediate_parents pyre_api
    | Pyrefly pyrefly_api -> PyreflyApi.ReadOnly.class_immediate_parents pyrefly_api


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


  let parse_reference = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.parse_reference pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.parse_reference"


  let class_exists = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.class_exists pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.class_exists"


  let get_define_body = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_define_body pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_define_body"


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


  let overrides = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.overrides pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.overrides"


  let annotation_parser = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.annotation_parser pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.annotation_parser"


  let typed_dictionary_field_names = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.typed_dictionary_field_names pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.typed_dictionary_field_names"


  let less_or_equal = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.less_or_equal pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.less_or_equal"


  let resolve_exports = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_exports pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_exports"


  let successors = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.successors pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.successors"


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


  let exists_matching_class_decorator = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.exists_matching_class_decorator pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.exists_matching_class_decorator"


  let generic_parameters_as_variables = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.generic_parameters_as_variables pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.generic_parameters_as_variables"


  let decorated_define = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.decorated_define pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.decorated_define"


  let named_tuple_attributes = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.named_tuple_attributes pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.named_tuple_attributes"


  let resolve_expression_to_type_info = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.resolve_expression_to_type_info pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.resolve_expression_to_type_info"


  let get_unannotated_global = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.get_unannotated_global pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.get_unannotated_global"


  let all_classes = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.all_classes pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.all_classes"


  let all_unannotated_globals = function
    | Pyre1 pyre_api -> Pyre1Api.ReadOnly.all_unannotated_globals pyre_api
    | Pyrefly _ -> failwith "unimplemented: ReadOnly.all_unannotated_globals"
end

module InContext = struct
  type t =
    | Pyre1 of Pyre1Api.InContext.t
    | Pyrefly of unit

  let create_at_global_scope = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1 (Pyre1Api.InContext.create_at_global_scope pyre_api)
    | ReadOnly.Pyrefly _ -> Pyrefly ()


  let create_at_statement_key api ~define_name ~define ~statement_key =
    match api with
    | ReadOnly.Pyre1 pyre_api ->
        Pyre1
          (Pyre1Api.InContext.create_at_statement_key pyre_api ~define_name ~define ~statement_key)
    | ReadOnly.Pyrefly _ -> failwith "unimplemented: InContext.create_at_statement_key"


  let pyre_api = function
    | Pyre1 pyre_context -> ReadOnly.Pyre1 (Pyre1Api.InContext.pyre_api pyre_context)
    | Pyrefly _ -> failwith "unimplemented: InContext.pyre_api"


  let is_global = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.is_global pyre_context
    | Pyrefly _ -> failwith "unimplemented: InContext.is_global"


  let resolve_reference = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_reference pyre_context
    | Pyrefly _ -> failwith "unimplemented: InContext.resolve_reference"


  let resolve_assignment api assign =
    match api with
    | Pyre1 pyre_context -> Pyre1 (Pyre1Api.InContext.resolve_assignment pyre_context assign)
    | Pyrefly _ -> failwith "unimplemented: InContext.resolve_assignment"


  let resolve_expression_to_type = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_expression_to_type pyre_context
    | Pyrefly _ -> failwith "unimplemented: InContext.resolve_expression_to_type"


  let resolve_attribute_access = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.resolve_attribute_access pyre_context
    | Pyrefly _ -> failwith "unimplemented: InContext.resolve_attribute_access"


  let fallback_attribute = function
    | Pyre1 pyre_context -> Pyre1Api.InContext.fallback_attribute pyre_context
    | Pyrefly _ -> failwith "unimplemented: InContext.fallback_attribute"


  let resolve_generators api generators =
    match api with
    | Pyre1 pyre_context -> Pyre1 (Pyre1Api.InContext.resolve_generators pyre_context generators)
    | Pyrefly _ -> failwith "unimplemented: InContext.resolve_generators"
end

module ModelQueries = struct
  let property_decorators = Pyre1Api.ModelQueries.property_decorators

  module Function = Pyre1Api.ModelQueries.Function
  module Global = Pyre1Api.ModelQueries.Global

  let resolve_qualified_name_to_global = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1Api.ModelQueries.resolve_qualified_name_to_global pyre_api
    | ReadOnly.Pyrefly pyrefly_api ->
        PyreflyApi.ModelQueries.resolve_qualified_name_to_global pyrefly_api


  let class_summaries = function
    | ReadOnly.Pyre1 pyre_api -> Pyre1Api.ModelQueries.class_summaries pyre_api
    | ReadOnly.Pyrefly _ -> failwith "unimplemented: ModelQueries.class_summaries"


  let invalidate_cache = function
    | ReadOnly.Pyre1 _ -> Pyre1Api.ModelQueries.invalidate_cache ()
    | ReadOnly.Pyrefly _ -> failwith "unimplemented: ModelQueries.invalidate_cache"
end
