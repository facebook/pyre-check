(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Ast
open Core
module Error = AnalysisError

module ReadOnly = struct
  type t = {
    global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
    get_errors: Reference.t -> Error.t list;
    get_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t option;
  }

  let create ?(get_errors = fun _ -> []) ?(get_local_annotations = fun _ -> None) global_environment
    =
    { global_environment; get_errors; get_local_annotations }


  let global_environment { global_environment; _ } = global_environment

  let global_resolution { global_environment; _ } = GlobalResolution.create global_environment

  let ast_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment


  let unannotated_global_environment { global_environment; _ } =
    AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment global_environment


  let get_errors { get_errors; _ } = get_errors

  let get_local_annotations { get_local_annotations; _ } = get_local_annotations

  let get_or_recompute_local_annotations environment name =
    match get_local_annotations environment name with
    | Some _ as local_annotations -> local_annotations
    | None ->
        (* Local annotations not preserved in shared memory in a standard pyre server (they can be,
           via TypeEnvironment.LocalAnnotations, but to save memory we only populate this for pysa
           runs, not the normal server used by LSP). This behavior is controlled by the
           `store_type_check_resolution` flag. *)
        let global_environment = global_environment environment in
        TypeCheck.compute_local_annotations ~global_environment name
end

module CheckResultValue = struct
  type t = TypeCheck.CheckResult.t

  let prefix = Prefix.make ()

  let description = "CheckResult"
end

module CheckResults =
  Memory.FirstClass.NoCache.Make (SharedMemoryKeys.ReferenceKey) (CheckResultValue)

type t = {
  global_environment: AnnotatedGlobalEnvironment.t;
  check_results: CheckResults.t;
}

let global_environment { global_environment; _ } = global_environment

let ast_environment { global_environment; _ } =
  AnnotatedGlobalEnvironment.ast_environment global_environment


let module_tracker type_environment =
  ast_environment type_environment |> AstEnvironment.module_tracker


let get_check_result { check_results; _ } reference = CheckResults.get check_results reference

let get_errors environment reference =
  get_check_result environment reference
  >>= TypeCheck.CheckResult.errors
  |> Option.value ~default:[]


let get_local_annotations environment reference =
  get_check_result environment reference >>= TypeCheck.CheckResult.local_annotations


let invalidate { check_results; _ } qualifiers =
  CheckResults.KeySet.of_list qualifiers |> CheckResults.remove_batch check_results


let from_global_environment global_environment =
  let check_results = CheckResults.create () in
  { global_environment; check_results }


let create configuration =
  AnnotatedGlobalEnvironment.create configuration |> from_global_environment


let create_for_testing configuration module_path_code_pairs =
  AnnotatedGlobalEnvironment.create_for_testing configuration module_path_code_pairs
  |> from_global_environment


let populate_for_definition
    ~configuration
    ~environment:{ global_environment; check_results }
    ?call_graph_builder
    (name, dependency)
  =
  TypeCheck.check_define_by_name
    ~configuration
    ~global_environment:(AnnotatedGlobalEnvironment.read_only global_environment)
    ?call_graph_builder
    (name, dependency)
  >>| CheckResults.add check_results name
  |> ignore


let populate_for_definitions ~scheduler ~configuration ?call_graph_builder environment defines =
  let timer = Timer.start () in

  let number_of_defines = List.length defines in
  Log.info "Checking %d functions..." number_of_defines;
  let map _ names =
    let analyze_define number_defines define_name_and_dependency =
      populate_for_definition
        ~configuration
        ~environment
        ?call_graph_builder
        define_name_and_dependency;
      number_defines + 1
    in
    List.fold names ~init:0 ~f:analyze_define
  in
  let reduce left right =
    let number_defines = left + right in
    Log.log ~section:`Progress "Processed %d of %d functions" number_defines number_of_defines;
    number_defines
  in
  let _ =
    SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunk_size:10
           ~minimum_chunks_per_worker:2
           ~preferred_chunk_size:1000
           ())
      ~initial:0
      ~map
      ~reduce
      ~inputs:defines
      ()
  in

  Statistics.performance ~name:"check_TypeCheck" ~phase_name:"Type check" ~timer ()


let populate_for_modules ~scheduler ~configuration ?call_graph_builder environment qualifiers =
  Profiling.track_shared_memory_usage ~name:"Before legacy type check" ();

  let all_defines =
    let unannotated_global_environment =
      global_environment environment
      |> AnnotatedGlobalEnvironment.read_only
      |> AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment
    in
    let map _ qualifiers =
      List.concat_map qualifiers ~f:(fun qualifier ->
          UnannotatedGlobalEnvironment.ReadOnly.get_define_names
            unannotated_global_environment
            qualifier)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~initial:[]
      ~map
      ~reduce:List.append
      ~inputs:qualifiers
      ()
  in
  let all_defines =
    match configuration with
    | { Configuration.Analysis.incremental_style = FineGrained; _ } ->
        List.map all_defines ~f:(fun define ->
            ( define,
              Some
                (SharedMemoryKeys.DependencyKey.Registry.register
                   (SharedMemoryKeys.TypeCheckDefine define)) ))
    | _ -> List.map all_defines ~f:(fun define -> define, None)
  in

  populate_for_definitions ~scheduler ~configuration ?call_graph_builder environment all_defines;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  Profiling.track_shared_memory_usage ~name:"After legacy type check" ()


let read_only ({ global_environment; _ } as environment) =
  ReadOnly.create
    ~get_errors:(get_errors environment)
    ~get_local_annotations:(get_local_annotations environment)
    (AnnotatedGlobalEnvironment.read_only global_environment)


(* All SharedMemory tables are populated and stored in separate, imperative steps that must be run
   before loading / after storing. These functions only handle serializing and deserializing the
   non-SharedMemory data *)

let store { global_environment; _ } =
  AnnotatedGlobalEnvironment.store global_environment;
  SharedMemoryKeys.DependencyKey.Registry.store ()


let load configuration =
  (* Loading the dependency keys needs to happen exactly once in the environment stack; we do it
     here, at the very top. *)
  SharedMemoryKeys.DependencyKey.Registry.load ();
  AnnotatedGlobalEnvironment.load configuration |> from_global_environment
