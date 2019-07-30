(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

type result = {
  module_tracker: Analysis.ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: Analysis.Environment.t;
  errors: Analysis.Error.t list;
}

type analyze_source_results = {
  errors: Analysis.Error.t list;
  number_files: int;
}
(** Internal result type; not exposed. *)

let analyze_sources
    ?(open_documents = Path.Map.empty)
    ~scheduler
    ~configuration
    ~environment
    source_paths
  =
  let open Analysis in
  Annotated.Class.AttributeCache.clear ();
  let checked_source_paths =
    List.filter source_paths ~f:(fun { SourcePath.is_external; _ } -> not is_external)
  in
  let errors =
    let timer = Timer.start () in
    let empty_result = { errors = []; number_files = 0 } in
    let number_of_sources = List.length checked_source_paths in
    Log.info "Checking %d sources..." number_of_sources;
    let run (module Check : Analysis.Check.Signature) =
      Log.info "Running check `%s`..." Check.name;
      let timer = Timer.start () in
      let map _ source_paths =
        Annotated.Class.AttributeCache.clear ();
        Module.Cache.clear ();
        let analyze_source { errors; number_files } ({ SourcePath.qualifier; _ } as source_path) =
          let path = SourcePath.full_path ~configuration source_path in
          match SharedMemory.Sources.get qualifier with
          | Some source ->
              let configuration =
                if PyrePath.Map.mem open_documents path then
                  { configuration with Configuration.Analysis.store_type_check_resolution = true }
                else
                  configuration
              in
              let global_resolution = Environment.resolution environment () in
              let new_errors = Check.run ~configuration ~global_resolution ~source in
              { errors = List.append new_errors errors; number_files = number_files + 1 }
          | _ -> { errors; number_files = number_files + 1 }
        in
        List.fold source_paths ~init:empty_result ~f:analyze_source
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
          ~inputs:checked_source_paths
          ()
      in
      Statistics.performance ~name:(Format.asprintf "check_%s" Check.name) ~timer ();
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size post-typecheck"
        ~integers:["size", SharedMemory.heap_size ()]
        ();
      errors
    in
    let errors = List.map (Analysis.Check.checks ~configuration) ~f:run |> List.concat in
    Statistics.performance ~name:"analyzed sources" ~timer ();
    errors
  in
  let timer = Timer.start () in
  let errors = Postprocess.ignore ~configuration scheduler source_paths errors in
  Statistics.performance ~name:"postprocessed" ~timer ();
  errors


let check
    ~scheduler:original_scheduler
    ~configuration:( { Configuration.Analysis.project_root; local_root; search_path; _ } as
                   configuration )
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
  in
  check_directory_exists local_root;
  check_directory_exists project_root;
  search_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  let scheduler =
    let bucket_multiplier =
      try
        Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> fun value -> Option.value_exn value)
      with
      | _ -> 10
    in
    match original_scheduler with
    | None -> Scheduler.create ~configuration ~bucket_multiplier ()
    | Some scheduler -> scheduler
  in
  (* Find sources to parse *)
  let module_tracker = Analysis.ModuleTracker.create configuration in
  (* Parse sources. *)
  let source_paths, ast_environment = Parser.parse_all ~scheduler ~configuration module_tracker in
  let environment = Environment.shared_handler in
  let () =
    let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
    Environment.populate_shared_memory ~configuration ~scheduler qualifiers
  in
  (* Do not count external files when computing ignores / checking types / computing coverages *)
  let source_paths =
    List.filter source_paths ~f:(fun { SourcePath.is_external; _ } -> not is_external)
  in
  Postprocess.register_ignores ~configuration scheduler source_paths;
  let errors = analyze_sources ~scheduler ~configuration ~environment source_paths in
  (* Log coverage results *)
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:local_root
    |> Option.value ~default:(Path.absolute local_root)
  in
  let open Analysis in
  let { Coverage.strict_coverage; declare_coverage; default_coverage; source_files } =
    let number_of_files = List.length source_paths in
    let sources = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
    Coverage.coverage ~sources ~number_of_files
  in
  let { Coverage.full; partial; untyped; ignore; crashes } =
    let aggregate sofar { SourcePath.qualifier; _ } =
      match Coverage.get ~qualifier with
      | Some coverage -> Coverage.sum sofar coverage
      | _ -> sofar
    in
    List.fold source_paths ~init:(Coverage.create ()) ~f:aggregate
  in
  Statistics.coverage
    ~randomly_log_every:20
    ~path:path_to_files
    ~coverage:
      [ "crashes", crashes;
        "declare_coverage", declare_coverage;
        "default_coverage", default_coverage;
        "full_type_coverage", full;
        "ignore_coverage", ignore;
        "no_type_coverage", untyped;
        "partial_type_coverage", partial;
        "source_files", source_files;
        "strict_coverage", strict_coverage;
        "total_errors", List.length errors ]
    ();

  (* Only destroy the scheduler if the check command created it. *)
  ( match original_scheduler with
  | None -> Scheduler.destroy scheduler
  | Some _ -> () );
  { module_tracker; ast_environment; environment; errors }
