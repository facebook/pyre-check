(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* FunctionDefinitionEnvironment stores the preprocessed asts for all of the functions (including
   the module toplevel and any class top levels). These are needed to perform pyre checks. They are
   stored separately from the UnannotatedGlobalEnvironment data because they are not used when
   computing type information for globals. *)

open Core
open Pyre
open Ast
module PreviousEnvironment = AttributeResolution

module IncomingDataComputation = struct
  let reference_map_of_alist_prefer_first =
    let prefer_left left _ = left in
    Reference.Map.Tree.of_alist_reduce ~f:prefer_left


  let function_definitions source_of_qualifier qualifier =
    source_of_qualifier qualifier
    >>| FunctionDefinition.collect_defines
    >>| reference_map_of_alist_prefer_first
end

module OutgoingDataComputation = struct
  let search_possible_containing_modules ~f reference =
    let ancestors_descending = Reference.possible_qualifiers_after_delocalize reference in
    List.find_map ~f ancestors_descending


  let define_names_of_qualifier function_definitions_of_qualifier qualifier =
    function_definitions_of_qualifier qualifier
    >>| Reference.Map.Tree.keys
    |> Option.value ~default:[]


  let function_definition function_definitions_of_qualifier function_name =
    let load_function_definition_if_in_module qualifier =
      function_definitions_of_qualifier qualifier
      >>= fun function_definitions -> Reference.Map.Tree.find function_definitions function_name
    in
    search_possible_containing_modules ~f:load_function_definition_if_in_module function_name
end

module FunctionDefinitionsTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.ReferenceKey

  module Value = struct
    type t = FunctionDefinition.t Reference.Map.Tree.t option [@@deriving equal]

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "FunctionDefinitions"
  end

  let produce_value annotated_global_environment key ~dependency =
    let source_code_api =
      AttributeResolution.ReadOnly.source_code_read_only annotated_global_environment
      |>
      match dependency with
      | None -> SourceCodeIncrementalApi.ReadOnly.get_untracked_api
      | Some dependency -> SourceCodeIncrementalApi.ReadOnly.get_tracked_api ~dependency
    in
    let source_of_qualifier = SourceCodeApi.source_of_qualifier source_code_api in
    IncomingDataComputation.function_definitions source_of_qualifier key


  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let show_key = Reference.show

  let overlay_owns_key source_code_overlay =
    SourceCodeIncrementalApi.Overlay.owns_qualifier source_code_overlay


  let equal_value = Value.equal

  let trigger_to_dependency qualifier = SharedMemoryKeys.FunctionDefinitions qualifier

  let filter_upstream_dependency = function
    | SharedMemoryKeys.FunctionDefinitions qualifier -> Some qualifier
    | _ -> None
end)

include FunctionDefinitionsTable

module FineGrainedUntrackedLogic = struct
  module SingleFunctionDefinitionValue = struct
    type t = FunctionDefinition.t option [@@deriving equal]

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "SingleFunctionDefinition"
  end

  module FunctionDefinitionNamesValue = struct
    type t = Reference.t list [@@deriving equal]

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "FunctionDefinitionNames"
  end

  module SingleFunctionDefinition = struct
    include Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (SingleFunctionDefinitionValue)
  end

  module FunctionDefinitionNames = struct
    include Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (FunctionDefinitionNamesValue)
  end

  let write_aside_single_function_definition ~key ~data =
    SingleFunctionDefinition.write_around key (Some data)


  let produce_value read_only qualifier =
    let upstream_read_only = ReadOnly.upstream_environment read_only in
    FunctionDefinitionsTable.In.produce_value ~dependency:None upstream_read_only qualifier


  let produce_and_write_aside read_only ?(force_load = true) qualifier =
    if force_load || FunctionDefinitionNames.get qualifier |> Option.is_none then
      match produce_value read_only qualifier with
      | None -> FunctionDefinitionNames.write_around qualifier []
      | Some function_definitions ->
          FunctionDefinitionNames.write_around
            qualifier
            (Reference.Map.Tree.keys function_definitions);
          Reference.Map.Tree.iteri ~f:write_aside_single_function_definition function_definitions;
          ();
          ()


  let produce_and_write_aside_function_definitions_all_parents read_only function_name =
    Reference.all_parents function_name
    |> List.iter ~f:(produce_and_write_aside ~force_load:false read_only)


  let define_names_of_qualifier read_only ?dependency:_ qualifier =
    match FunctionDefinitionNames.get qualifier with
    | Some hit -> hit
    | None -> (
        let () = produce_and_write_aside read_only qualifier in
        match FunctionDefinitionNames.get qualifier with
        | Some hit -> hit
        | None -> failwith "Should be impossible")


  let function_definition read_only ?dependency:_ function_name =
    match SingleFunctionDefinition.get function_name with
    | Some hit -> hit
    | None -> (
        let () = produce_and_write_aside_function_definitions_all_parents read_only function_name in
        match SingleFunctionDefinition.get function_name with
        | Some hit -> hit
        | None ->
            SingleFunctionDefinition.write_around function_name None;
            None)
end

module DependencyTrackedLogic = struct
  let function_definitions_of_qualifier read_only ?dependency qualifier =
    ReadOnly.get read_only ?dependency qualifier


  let define_names_of_qualifier read_only ?dependency qualifier =
    let function_definitions_of_qualifier =
      function_definitions_of_qualifier read_only ?dependency
    in
    OutgoingDataComputation.define_names_of_qualifier function_definitions_of_qualifier qualifier


  let function_definition read_only ?dependency function_name =
    let function_definitions_of_qualifier =
      function_definitions_of_qualifier read_only ?dependency
    in
    OutgoingDataComputation.function_definition function_definitions_of_qualifier function_name
end

module FunctionDefinitionReadOnly = struct
  include FunctionDefinitionsTable.ReadOnly

  let track_dependencies read_only =
    let { Configuration.Analysis.track_dependencies; _ } =
      controls read_only |> EnvironmentControls.configuration
    in
    track_dependencies


  let define_names_of_qualifier read_only =
    if track_dependencies read_only then
      DependencyTrackedLogic.define_names_of_qualifier read_only
    else
      FineGrainedUntrackedLogic.define_names_of_qualifier read_only


  let function_definition read_only =
    if track_dependencies read_only then
      DependencyTrackedLogic.function_definition read_only
    else
      FineGrainedUntrackedLogic.function_definition read_only


  let attribute_resolution = upstream_environment
end

module ReadOnly = FunctionDefinitionReadOnly
