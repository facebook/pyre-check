(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TypeEnvironment: layer of the environment stack
 * - upstream: AnnotatedGlobalEnvironment
 *   - all upstream logic is actually accessed, in typeCheck.ml code,
 *     through GlobalResolution which provides an interface to lower
 *     layers.
 * - downstream: ErrorsEnvironment
 * - key: name of a define, as a Reference.t
 *   - module and class toplevels are included here!
 * - value: TypeCheck.CheckResult.t, which has two optional fields:
 *   - a list of type errors per define
 *   - a map of local annotation information about variable types at each
 *     point in the control flow graph
 *)

open Pyre
open Ast
open Core
module PreviousEnvironment = AnnotatedGlobalEnvironment

module CheckResultValue = struct
  type t = TypeCheck.CheckResult.t option [@@deriving equal]

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "CheckResult"
end

let produce_check_results global_environment define_name ~dependency =
  let type_check_controls, call_graph_builder, dependency =
    let controls = AnnotatedGlobalEnvironment.ReadOnly.controls global_environment in
    let type_check_controls = EnvironmentControls.type_check_controls controls in
    let call_graph_builder =
      if EnvironmentControls.populate_call_graph controls then
        (module Callgraph.DefaultBuilder : Callgraph.Builder)
      else
        (module Callgraph.NullBuilder : Callgraph.Builder)
    in
    let dependency =
      if EnvironmentControls.track_dependencies controls then
        dependency
      else
        None
    in
    type_check_controls, call_graph_builder, dependency
  in
  TypeCheck.check_define_by_name
    ~type_check_controls
    ~call_graph_builder
    ~global_environment
    ~dependency
    define_name


module CheckResultsTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = AnnotatedGlobalEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = CheckResultValue

  type trigger = Reference.t [@@deriving sexp, compare]

  module TriggerSet = Reference.Set

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  let show_key = Reference.show

  let overlay_owns_key source_code_overlay =
    SourceCodeIncrementalApi.Overlay.owns_reference source_code_overlay


  let lazy_incremental = false

  let produce_value = produce_check_results

  let filter_upstream_dependency = function
    | SharedMemoryKeys.TypeCheckDefine name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.TypeCheckDefine name

  let equal_value = CheckResultValue.equal
end)

include CheckResultsTable

let global_environment = CheckResultsTable.AssumeDownstreamNeverNeedsUpdates.upstream

module AssumeGlobalModuleListing = struct
  let global_module_paths_api type_environment =
    source_code_base type_environment
    |> SourceCodeIncrementalApi.Base.AssumeGlobalModuleListing.global_module_paths_api
end

let populate_for_definitions ~scheduler ~scheduler_policies environment defines =
  let timer = Timer.start () in

  let read_only = read_only environment in
  let number_of_defines = List.length defines in
  Log.info "Checking %d functions..." number_of_defines;
  let map names =
    let analyze_define (number_defines, number_errors) name =
      let check_result = ReadOnly.get read_only name in
      let define_errors =
        match check_result with
        | Some { TypeCheck.CheckResult.errors = Some errors; _ } -> List.length errors
        | _ -> 0
      in
      number_defines + 1, number_errors + define_errors
    in
    List.fold names ~init:(0, 0) ~f:analyze_define
  in
  let reduce (left_defines, left_errors) (right_defines, right_errors) =
    let number_defines = left_defines + right_defines in
    let number_errors = left_errors + right_errors in
    Log.log ~section:`Progress "Processed %d of %d functions" number_defines number_of_defines;
    number_defines, number_errors
  in
  let scheduler_policy =
    Scheduler.Policy.from_configuration_or_default
      scheduler_policies
      Configuration.ScheduleIdentifier.TypeCheck
      ~default:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunk_size:1
           ~minimum_chunks_per_worker:2
           ~preferred_chunk_size:5000
           ())
  in
  let number_defines, number_errors =
    SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:(0, 0)
      ~map
      ~reduce
      ~inputs:defines
      ()
  in

  Statistics.performance
    ~name:"check_TypeCheck"
    ~phase_name:CheckResultValue.description
    ~timer
    ~integers:["defines", number_defines; "raw errors", number_errors]
    ()


let collect_definitions ~scheduler ~scheduler_policies environment qualifiers =
  let timer = Timer.start () in
  Log.info "Collecting all definitions...";
  let function_definition_environment =
    global_environment environment
    |> AnnotatedGlobalEnvironment.read_only
    |> AnnotatedGlobalEnvironment.ReadOnly.function_definition_environment
  in
  let map qualifiers =
    List.concat_map qualifiers ~f:(fun qualifier ->
        FunctionDefinitionEnvironment.ReadOnly.define_names_of_qualifier
          function_definition_environment
          qualifier)
  in
  let scheduler_policy =
    Scheduler.Policy.from_configuration_or_default
      scheduler_policies
      Configuration.ScheduleIdentifier.CollectDefinitions
      ~default:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:1
           ~preferred_chunks_per_worker:1
           ())
  in
  let defines =
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map
      ~reduce:List.append
      ~inputs:qualifiers
      ()
  in
  Statistics.performance
    ~name:"collected definitions"
    ~timer
    ~integers:["defines", List.length defines]
    ();
  defines


let populate_for_modules ~scheduler ~scheduler_policies environment qualifiers =
  PyreProfiling.track_shared_memory_usage ~name:"Before legacy type check" ();
  let all_defines = collect_definitions ~scheduler ~scheduler_policies environment qualifiers in
  populate_for_definitions ~scheduler ~scheduler_policies environment all_defines;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  PyreProfiling.track_shared_memory_usage ~name:"After legacy type check" ()


module ReadOnly = struct
  include CheckResultsTable.ReadOnly

  let global_environment = CheckResultsTable.Testing.ReadOnly.upstream

  let global_resolution environment = global_environment environment |> GlobalResolution.create

  let function_definition_environment environment =
    global_environment environment
    |> AnnotatedGlobalEnvironment.ReadOnly.function_definition_environment


  let unannotated_global_environment read_only =
    global_environment read_only
    |> AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment


  let get_tracked_source_code_api environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.get_tracked_api


  let get_untracked_source_code_api environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.get_untracked_api


  let get_environment_controls environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.controls


  let get_errors environment ?dependency reference =
    get ?dependency environment reference
    >>= TypeCheck.CheckResult.errors
    |> Option.value ~default:[]


  let get_local_annotations environment ?dependency define_name define_location =
    get ?dependency environment define_name
    >>= TypeCheck.CheckResult.local_annotations ~define_location


  let get_callees environment ?dependency reference =
    get ?dependency environment reference >>= TypeCheck.CheckResult.callees


  let get_or_recompute_local_annotations environment ?dependency define_name define_location =
    match get_local_annotations ?dependency environment define_name define_location with
    | Some _ as local_annotations -> local_annotations
    | None ->
        (* Local annotations not preserved in shared memory in a standard pyre server (they can be,
           via TypeEnvironment.LocalAnnotations, but to save memory we only populate this for pysa
           runs, not the normal server used by LSP). This behavior is controlled by the
           `store_type_check_resolution` flag. *)
        let global_resolution = global_resolution environment in
        let type_check_controls =
          get_environment_controls environment |> EnvironmentControls.type_check_controls
        in
        TypeCheck.compute_local_annotations
          ~type_check_controls
          ~global_resolution
          ~define_name
          ~define_location
        >>| fun (local_annotations, _expression_types) -> local_annotations
end

module AssumeAstEnvironment = struct
  (* All SharedMemory tables are populated and stored in separate, imperative steps that must be run
     before loading / after storing. These functions only handle serializing and deserializing the
     non-SharedMemory data *)

  let store environment =
    CheckResultsTable.AssumeAstEnvironment.store environment;
    SharedMemoryKeys.DependencyKey.Registry.store ()


  let load configuration =
    (* Loading the dependency keys needs to happen exactly once in the environment stack; we do it
       here, at the very top. *)
    SharedMemoryKeys.DependencyKey.Registry.load ();
    CheckResultsTable.AssumeAstEnvironment.load configuration


  let store_without_dependency_keys = CheckResultsTable.AssumeAstEnvironment.store

  let load_without_dependency_keys = CheckResultsTable.AssumeAstEnvironment.load

  (* Pysa sometimes directly relies on the AstEnvironment (mainly for cache logic rather than the
     core business logic) and we therefore need this handle. Some legacy testing code likelwise
     needs direct access to the ast environment in order to wipe shared memory between tests. *)

  let ast_environment environment =
    global_environment environment
    |> AnnotatedGlobalEnvironment.AssumeDownstreamNeverNeedsUpdates.upstream
    |> FunctionDefinitionEnvironment.AssumeDownstreamNeverNeedsUpdates.upstream
    |> AttributeResolution.AssumeDownstreamNeverNeedsUpdates.upstream
    |> ClassSuccessorMetadataEnvironment.AssumeDownstreamNeverNeedsUpdates.upstream
    |> ClassHierarchyEnvironment.AssumeDownstreamNeverNeedsUpdates.upstream
    |> TypeAliasEnvironment.AssumeDownstreamNeverNeedsUpdates.upstream
    |> UnannotatedGlobalEnvironment.AssumeAstEnvironment.ast_environment
end

module TypeEnvironmentReadOnly = ReadOnly
