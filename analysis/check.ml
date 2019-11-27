(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
module Error = AnalysisError
open Pyre

module type Signature = sig
  val name : string

  val run
    :  configuration:Configuration.Analysis.t ->
    environment:TypeEnvironment.ReadOnly.t ->
    source:Source.t ->
    Error.t list
end

let checks : (module Signature) String.Map.t =
  let checks : (string * (module Signature)) list =
    [
      "awaitable", (module AwaitableCheck);
      "deobfuscation", (module DeobfuscationCheck);
      "immutable_collection", (module ImmutableCollectionCheck);
      "liveness", (module LivenessCheck);
    ]
  in
  String.Map.of_alist_exn checks


let get_check_to_run ~check_name = Map.find checks check_name

let run_type_check ?open_documents ~scheduler ~configuration ~environment checked_sources =
  let number_of_sources = List.length checked_sources in
  Log.info "Running type check...";
  let timer = Timer.start () in
  let map _ qualifiers =
    AttributeResolution.AttributeCache.clear ();
    let analyze_source
        number_files
        ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
      =
      let configuration =
        match open_documents with
        | Some predicate when predicate qualifier ->
            { configuration with Configuration.Analysis.store_type_check_resolution = true }
        | _ -> configuration
      in
      TypeCheck.run ~configuration ~environment ~source;
      number_files + 1
    in
    let ast_environment = TypeEnvironment.ast_environment environment in
    List.filter_map qualifiers ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
    |> List.fold ~init:0 ~f:analyze_source
  in
  let reduce left right =
    let number_files = left + right in
    Log.log ~section:`Progress "Processed %d of %d sources" number_files number_of_sources;
    number_files
  in
  let _ =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~bucket_size:75
      ~initial:0
      ~map
      ~reduce
      ~inputs:checked_sources
      ()
  in
  Statistics.performance ~name:"check_TypeCheck" ~timer ();
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  ()


let analyze_sources
    ?open_documents
    ?(filter_external_sources = true)
    ~scheduler
    ~configuration
    ~environment
    sources
  =
  let ast_environment = TypeEnvironment.ast_environment environment in
  AttributeResolution.AttributeCache.clear ();
  let checked_sources =
    if filter_external_sources then
      let is_not_external qualifier =
        AstEnvironment.ReadOnly.get_source_path ast_environment qualifier
        >>| (fun { SourcePath.is_external; _ } -> not is_external)
        |> Option.value ~default:false
      in
      List.filter sources ~f:is_not_external
    else
      sources
  in
  let number_of_sources = List.length checked_sources in
  Log.info "Checking %d sources..." number_of_sources;
  Profiling.track_shared_memory_usage ~name:"Before analyze_sources" ();
  let timer = Timer.start () in
  run_type_check ?open_documents ~scheduler ~configuration ~environment checked_sources;
  Statistics.performance ~name:"analyzed sources" ~phase_name:"Type check" ~timer ();
  Profiling.track_shared_memory_usage ~name:"After analyze_sources" ()


let postprocess_sources ~scheduler ~configuration ~environment sources =
  Log.log ~section:`Progress "Postprocessing...";
  let map _ modules = Postprocessing.run ~modules environment in
  let reduce = List.append in
  Scheduler.map_reduce
    scheduler
    ~configuration
    ~bucket_size:200
    ~initial:[]
    ~map
    ~reduce
    ~inputs:sources
    ()


let analyze_and_postprocess
    ?open_documents
    ?filter_external_sources
    ~scheduler
    ~configuration
    ~environment
    sources
  =
  analyze_sources
    ?open_documents
    ?filter_external_sources
    ~scheduler
    ~configuration
    ~environment
    sources;
  postprocess_sources
    ~scheduler
    ~configuration
    ~environment:(TypeEnvironment.read_only environment)
    sources
