(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module AnalysisErrorValue = struct
  type t = Error.t list

  let prefix = Prefix.make ()

  let description = "Raw analysis errors"
end

module LocalAnnotationsValue = struct
  type t = LocalAnnotationMap.ReadOnly.t

  let prefix = Prefix.make ()

  let description = "Node type resolution"
end

module RawErrors =
  Memory.FirstClass.NoCache.Make (SharedMemoryKeys.ReferenceKey) (AnalysisErrorValue)
module LocalAnnotations =
  Memory.FirstClass.WithCache.Make (SharedMemoryKeys.ReferenceKey) (LocalAnnotationsValue)

type t = {
  global_environment: AnnotatedGlobalEnvironment.t;
  set_errors: Reference.t -> Error.t list -> unit;
  set_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t -> unit;
  invalidate: Reference.t list -> unit;
  get_errors: Reference.t -> Error.t list;
  get_local_annotations: Reference.t -> LocalAnnotationMap.ReadOnly.t option;
}

let global_environment { global_environment; _ } = global_environment

let ast_environment { global_environment; _ } =
  AnnotatedGlobalEnvironment.ast_environment global_environment


let module_tracker type_environment =
  ast_environment type_environment |> AstEnvironment.module_tracker


let set_errors { set_errors; _ } = set_errors

let get_errors { get_errors; _ } = get_errors

let set_local_annotations { set_local_annotations; _ } = set_local_annotations

let get_local_annotations { get_local_annotations; _ } = get_local_annotations

let invalidate { invalidate; _ } = invalidate

let from_global_environment global_environment =
  let raw_errors = RawErrors.create () in
  let local_annotations = LocalAnnotations.create () in
  let get_errors reference = RawErrors.get raw_errors reference |> Option.value ~default:[] in
  let set_errors = RawErrors.add raw_errors in
  let get_local_annotations = LocalAnnotations.get local_annotations in
  let set_local_annotations = LocalAnnotations.add local_annotations in
  let invalidate qualifiers =
    RawErrors.KeySet.of_list qualifiers |> RawErrors.remove_batch raw_errors;
    LocalAnnotations.KeySet.of_list qualifiers |> LocalAnnotations.remove_batch local_annotations
  in
  {
    global_environment;
    set_errors;
    set_local_annotations;
    invalidate;
    get_errors;
    get_local_annotations;
  }


let create configuration =
  AnnotatedGlobalEnvironment.create configuration |> from_global_environment


let create_for_testing configuration source_path_code_pairs =
  AnnotatedGlobalEnvironment.create_for_testing configuration source_path_code_pairs
  |> from_global_environment


let populate_for_definition ~configuration ~environment ?call_graph_builder (name, dependency) =
  let global_environment = global_environment environment |> AnnotatedGlobalEnvironment.read_only in
  match
    TypeCheck.check_define_by_name
      ~configuration
      ~global_environment
      ?call_graph_builder
      (name, dependency)
  with
  | None -> ()
  | Some { TypeCheck.CheckResult.errors; local_annotations } ->
      (if configuration.store_type_check_resolution then
         (* Write fixpoint type resolutions to shared memory *)
         let local_annotations =
           match local_annotations with
           | Some local_annotations -> local_annotations
           | None -> LocalAnnotationMap.empty ()
         in
         set_local_annotations environment name (LocalAnnotationMap.read_only local_annotations));
      if configuration.store_type_errors then
        set_errors environment name errors;
      ()


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


let read_only { global_environment; get_errors; get_local_annotations; _ } =
  ReadOnly.create
    ~get_errors
    ~get_local_annotations
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
