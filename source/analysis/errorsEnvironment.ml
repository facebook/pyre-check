(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
open Pyre
module PreviousEnvironment = TypeEnvironment
module Error = AnalysisError

module QualifierErrorsValue = struct
  type t = Error.t list [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "QualifierErrorsValue"

  let equal = Memory.equal_from_compare compare
end

let produce_errors type_environment qualifier ~dependency =
  Postprocessing.run_on_qualifier type_environment ~dependency qualifier


module QualifierErrorsTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = TypeEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = QualifierErrorsValue

  type trigger = Reference.t [@@deriving sexp, compare]

  module TriggerSet = Reference.Set

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  let show_key = Reference.show

  let overlay_owns_key module_tracker_overlay =
    ModuleTracker.Overlay.owns_qualifier module_tracker_overlay


  let lazy_incremental = false

  let produce_value = produce_errors

  let filter_upstream_dependency = function
    | SharedMemoryKeys.CreateModuleErrors name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.CreateModuleErrors name

  let equal_value = QualifierErrorsValue.equal
end)

include QualifierErrorsTable

module ReadOnly = struct
  include ReadOnly

  let type_environment environment = upstream_environment environment

  let ast_environment environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


  let module_tracker environment =
    ast_environment environment |> AstEnvironment.ReadOnly.module_tracker


  let get_errors_for_qualifier environment qualifier = get environment qualifier

  let get_all_errors environment =
    module_tracker environment
    |> ModuleTracker.ReadOnly.project_qualifiers
    |> List.concat_map ~f:(get_errors_for_qualifier environment)
end

let type_environment = Unsafe.upstream

let module_tracker environment = ast_environment environment |> AstEnvironment.module_tracker

module ErrorsEnvironmentReadOnly = ReadOnly

let populate_all_errors ~scheduler environment =
  (* Because of lazy evaluation, we can actually perform this operation using only a read-only
     environment. But we put it on the read-write API because the behavior is explicitly stateful. *)
  let environment = read_only environment in
  let timer = Timer.start () in
  let qualifiers =
    ReadOnly.module_tracker environment |> ModuleTracker.ReadOnly.project_qualifiers
  in
  let number_of_qualifiers = List.length qualifiers in
  Log.log ~section:`Progress "Postprocessing %d sources..." number_of_qualifiers;
  let map _ modules =
    List.length modules, List.concat_map modules ~f:(ReadOnly.get_errors_for_qualifier environment)
  in
  let reduce (left_count, left_errors) (right_count, right_errors) =
    let number_so_far = left_count + right_count in
    Log.log ~section:`Progress "Postprocessed %d of %d sources" number_so_far number_of_qualifiers;
    number_so_far, List.append left_errors right_errors
  in
  let _ =
    SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~initial:(0, [])
      ~map
      ~reduce
      ~inputs:qualifiers
      ()
  in
  Statistics.performance ~name:"check_Postprocessing" ~phase_name:"Postprocessing" ~timer ();
  ()


module UpdateStatistics = struct
  type t = {
    module_updates_count: int;
    invalidated_modules_count: int;
    (* This includes only re-checks of previously existing functions, not checks of newly added
       functions *)
    rechecked_modules_count: int;
    rechecked_functions_count: int;
  }

  let count_updates update_result =
    let rechecked_functions_count, rechecked_modules_count =
      let rechecked_functions, rechecked_modules =
        let filter_union sofar keyset =
          let collect_unique registered (sofar_functions, sofar_modules) =
            match SharedMemoryKeys.DependencyKey.get_key registered with
            | SharedMemoryKeys.TypeCheckDefine name -> Set.add sofar_functions name, sofar_modules
            | SharedMemoryKeys.CreateModuleErrors name ->
                sofar_functions, Set.add sofar_modules name
            | _ -> sofar_functions, sofar_modules
          in
          SharedMemoryKeys.DependencyKey.RegisteredSet.fold collect_unique keyset sofar
        in
        UpdateResult.all_triggered_dependencies update_result
        |> List.fold ~init:(Reference.Set.empty, Reference.Set.empty) ~f:filter_union
      in
      Reference.Set.length rechecked_functions, Reference.Set.length rechecked_modules
    in
    let module_updates_count, invalidated_modules_count =
      let unannotated_global_environment_update_result =
        UpdateResult.unannotated_global_environment_update_result update_result
      in
      ( UnannotatedGlobalEnvironment.UpdateResult.module_updates
          unannotated_global_environment_update_result
        |> List.length,
        UnannotatedGlobalEnvironment.UpdateResult.invalidated_modules
          unannotated_global_environment_update_result
        |> List.length )
    in
    {
      module_updates_count;
      invalidated_modules_count;
      rechecked_modules_count;
      rechecked_functions_count;
    }
end

module Testing = struct
  module ReadOnly = struct
    include QualifierErrorsTable.Testing.ReadOnly

    let errors_environment = Fn.id

    let type_environment = ReadOnly.type_environment

    let annotated_global_environment environment =
      type_environment environment |> TypeEnvironment.Testing.ReadOnly.upstream


    let attribute_resolution environment =
      annotated_global_environment environment
      |> AnnotatedGlobalEnvironment.Testing.ReadOnly.upstream


    let class_metadata_environment environment =
      attribute_resolution environment |> AttributeResolution.Testing.ReadOnly.upstream


    let class_hierarchy_environment environment =
      class_metadata_environment environment |> ClassMetadataEnvironment.Testing.ReadOnly.upstream


    let alias_environment environment =
      class_hierarchy_environment environment |> ClassHierarchyEnvironment.Testing.ReadOnly.upstream


    let empty_stub_environment environment =
      alias_environment environment |> AliasEnvironment.Testing.ReadOnly.upstream


    let unannotated_global_environment environment =
      empty_stub_environment environment |> EmptyStubEnvironment.Testing.ReadOnly.upstream
  end

  module UpdateResult = struct
    include QualifierErrorsTable.Testing.UpdateResult

    let errors_environment = Fn.id

    let type_environment update_result = upstream update_result

    let annotated_global_environment update_result =
      type_environment update_result |> TypeEnvironment.Testing.UpdateResult.upstream


    let attribute_resolution update_result =
      annotated_global_environment update_result
      |> AnnotatedGlobalEnvironment.Testing.UpdateResult.upstream


    let class_metadata_environment update_result =
      attribute_resolution update_result |> AttributeResolution.Testing.UpdateResult.upstream


    let class_hierarchy_environment update_result =
      class_metadata_environment update_result
      |> ClassMetadataEnvironment.Testing.UpdateResult.upstream


    let alias_environment update_result =
      class_hierarchy_environment update_result
      |> ClassHierarchyEnvironment.Testing.UpdateResult.upstream


    let empty_stub_environment update_result =
      alias_environment update_result |> AliasEnvironment.Testing.UpdateResult.upstream


    let unannotated_global_environment update_result =
      empty_stub_environment update_result |> EmptyStubEnvironment.Testing.UpdateResult.upstream
  end
end

let create controls =
  let timer = Timer.start () in
  EnvironmentControls.configuration controls |> Configuration.Analysis.validate_paths;
  Profiling.track_shared_memory_usage ~name:"Before module tracking" ();
  Log.info "Creating environment...";
  let environment = create controls in
  Statistics.performance ~name:"full environment built" ~timer ();
  environment


let check_and_preprocess environment ~scheduler =
  type_environment environment |> TypeEnvironment.populate_for_project_modules ~scheduler;
  populate_all_errors ~scheduler environment;
  Profiling.track_shared_memory_usage ~name:"After checking and preprocessing" ();
  ()
