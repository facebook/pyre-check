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
    environment:AnnotatedGlobalEnvironment.ReadOnly.t ->
    source:Source.t ->
    Error.t list
end

let checks : (module Signature) String.Map.t =
  let checks : (string * (module Signature)) list =
    [
      "awaitable", (module AwaitableCheck);
      "deobfuscation", (module DeobfuscationCheck);
      "immutable_collection", (module ImmutableCollectionCheck);
      "inference", (module Inference);
      "liveness", (module LivenessCheck);
      "typeCheck", (module TypeCheck);
    ]
  in
  String.Map.of_alist_exn checks


let get_check_to_run ~check_name = Map.find checks check_name

let create_check ~configuration:{ Configuration.Analysis.infer; additional_checks; _ }
    : (module Signature)
  =
  let checks_to_run = if infer then ["inference"] else "typeCheck" :: additional_checks in
  let find name =
    match Map.find checks name with
    | Some check -> Some check
    | None ->
        Log.warning "Could not find check `%s`." name;
        None
  in
  let filtered_checks = List.filter_map checks_to_run ~f:find in
  let module AggregatedCheck : Signature = struct
    let name = String.concat checks_to_run ~sep:", "

    let run ~configuration ~environment ~source =
      let run_one_check (module Check : Signature) =
        Check.run ~configuration ~environment ~source
      in
      List.concat_map filtered_checks ~f:run_one_check
  end
  in
  (module AggregatedCheck)


type analyze_source_results = {
  errors: (Source.t * Error.t list) list;
  number_files: int;
}
(** Internal result type; not exposed. *)

let run_check
    ?open_documents
    ~scheduler
    ~configuration
    ~environment
    checked_sources
    (module Check : Signature)
  =
  let empty_result = { errors = []; number_files = 0 } in
  let number_of_sources = List.length checked_sources in
  Log.info "Running check `%s`..." Check.name;
  let timer = Timer.start () in
  let map _ qualifiers =
    Annotated.Class.AttributeCache.clear ();
    AstEnvironment.FromEmptyStubCache.clear ();
    let analyze_source
        { errors; number_files }
        ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
      =
      let configuration =
        match open_documents with
        | Some predicate when predicate qualifier ->
            { configuration with Configuration.Analysis.store_type_check_resolution = true }
        | _ -> configuration
      in
      let new_errors = Check.run ~configuration ~environment ~source in
      { errors = (source, new_errors) :: errors; number_files = number_files + 1 }
    in
    let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
    List.filter_map qualifiers ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
    |> List.fold ~init:empty_result ~f:analyze_source
  in
  let reduce left right =
    let number_files = left.number_files + right.number_files in
    Log.log ~section:`Progress "Processed %d of %d sources" number_files number_of_sources;
    { errors = List.append left.errors right.errors; number_files }
  in
  let { errors; _ } =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~bucket_size:75
      ~initial:empty_result
      ~map
      ~reduce
      ~inputs:checked_sources
      ()
  in
  Log.log ~section:`Progress "Postprocessing...";
  let map _ source_errors =
    List.concat_map source_errors ~f:(fun (source, error) -> Postprocessing.run ~source error)
  in
  let reduce = List.append in
  let errors =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~bucket_size:200
      ~initial:[]
      ~map
      ~reduce
      ~inputs:errors
      ()
  in
  Statistics.performance ~name:(Format.asprintf "check_%s" Check.name) ~timer ();
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  errors


let analyze_sources
    ?open_documents
    ?(filter_external_sources = true)
    ~scheduler
    ~configuration
    ~environment
    sources
  =
  let ast_environment = AnnotatedGlobalEnvironment.ReadOnly.ast_environment environment in
  Annotated.Class.AttributeCache.clear ();
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
  let errors =
    let check = create_check ~configuration in
    run_check ?open_documents ~scheduler ~configuration ~environment checked_sources check
  in
  Statistics.performance ~name:"analyzed sources" ~phase_name:"Type check" ~timer ();
  Profiling.track_shared_memory_usage ~name:"After analyze_sources" ();
  errors
