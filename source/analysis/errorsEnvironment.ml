(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core
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

  let get_all_errors_map_reduce ~scheduler environment =
    let timer = Timer.start () in
    let qualifiers = module_tracker environment |> ModuleTracker.ReadOnly.project_qualifiers in
    let number_of_qualifiers = List.length qualifiers in
    Log.log ~section:`Progress "Postprocessing %d sources..." number_of_qualifiers;
    let map _ modules =
      List.length modules, List.concat_map modules ~f:(get_errors_for_qualifier environment)
    in
    let reduce (left_count, left_errors) (right_count, right_errors) =
      let number_so_far = left_count + right_count in
      Log.log ~section:`Progress "Postprocessed %d of %d sources" number_so_far number_of_qualifiers;
      number_so_far, List.append left_errors right_errors
    in
    let _, errors =
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
    errors


  let get_all_errors environment =
    module_tracker environment
    |> ModuleTracker.ReadOnly.project_qualifiers
    |> List.concat_map ~f:(get_errors_for_qualifier environment)
end

let type_environment = Unsafe.upstream

let module_tracker environment = ast_environment environment |> AstEnvironment.module_tracker

module ErrorsEnvironmentReadOnly = ReadOnly
