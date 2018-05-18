(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre
open PyreParser

module Scheduler = ServiceScheduler
module AstSharedMemory = ServiceAstSharedMemory


let parse_path_to_source file =
  File.path file |> Path.relative
  >>= fun path ->
  File.lines file
  >>= fun lines ->
  let metadata = Source.Metadata.parse path lines in
  try
    let statements = Parser.parse ~path lines in
    Some (
      Source.create
        ~docstring:(Statement.extract_docstring statements)
        ~metadata
        ~path
        ~qualifier:(Source.qualifier ~path)
        statements)
  with
  | Parser.Error error ->
      Log.log ~section:`Parser "%s" error;
      None
  | Failure error ->
      Log.error "%s" error;
      None


let parse_parallel ~scheduler ~job ~files =
  Scheduler.map_reduce
    scheduler
    ~init:[]
    ~map:(fun _ files -> job ~files)
    ~reduce:(fun new_sources parsed_sources -> parsed_sources @ new_sources)
    files


(** Adds the preprocessing for stubs *)
let parse_stub_job ~configuration:{ Configuration.verbose; sections; _ } ~files =
  Log.initialize ~verbose ~sections;
  let parse_stub handles file =
    (file
     |> parse_path_to_source
     >>= fun source ->
     Path.relative (File.path file)
     >>| fun relative ->
     let preprocess source =
       let source =
         (* Drop version from qualifier. *)
         let qualifier =
           let is_digit qualifier =
             try
               qualifier
               |> Int.of_string
               |> ignore;
               true
             with _ ->
               false
           in
           match source.Source.qualifier with
           | minor :: major :: tail
             when is_digit (Expression.Access.show [minor]) &&
                  is_digit (Expression.Access.show [major]) ->
               tail
           | major :: tail when is_digit (String.prefix (Expression.Access.show [major]) 1) ->
               tail
           | qualifier ->
               qualifier
         in
         { source with Source.qualifier }
       in
       Analysis.Preprocessing.preprocess source
     in
     let handle = File.Handle.create relative in
     preprocess source
     |> AstSharedMemory.add_source handle;
     handle :: handles)
    |> Option.value ~default:handles
  in
  List.fold ~init:[] ~f:parse_stub files


let parse_stubs_list ~configuration ~scheduler ~files =
  let handles =
    if Scheduler.is_parallel scheduler then
      parse_parallel ~scheduler ~job:(parse_stub_job ~configuration) ~files
    else
      parse_stub_job ~configuration ~files
  in
  handles


let parse_stubs
    scheduler
    ~configuration:({ Configuration.source_root; stub_roots; _ } as configuration) =
  let timer = Timer.start () in

  let paths =
    let stubs =
      let stubs root =
        Log.info "Finding type stubs in `%a`..." Path.pp root;
        let is_stub path =
          let is_python_2_stub path =
            String.is_substring ~substring:"/2/" path ||
            String.is_substring ~substring:"/2.7/" path
          in
          String.is_suffix path ~suffix:".pyi" && not (is_python_2_stub path)
        in
        File.list ~filter:is_stub ~root
      in
      List.concat_map ~f:stubs (source_root :: stub_roots)
    in
    let modules =
      let modules root =
        Log.info "Finding external sources in `%a`..." Path.pp root;
        File.list ~filter:(String.is_suffix ~suffix:".py") ~root
      in
      List.concat_map ~f:modules stub_roots
    in
    stubs @ modules
  in

  Log.info "Parsing %d stubs and external sources..." (List.length paths);
  let handles = parse_stubs_list ~configuration ~scheduler ~files:(List.map ~f:File.create paths) in
  Statistics.performance ~name:"stubs parsed" ~timer ~configuration ();
  let not_parsed = (List.length paths) - (List.length handles) in
  if not_parsed > 0 then
    Log.debug "Unable to parse %d stubs or external sources" not_parsed;
  handles


(** Adds the preprocessing for sources *)
let parse_source_job ~configuration:{ Configuration.verbose; sections; _ } ~files =
  Log.initialize ~verbose ~sections;
  let parse_source handles file =
    (parse_path_to_source file
     >>= fun source ->
     File.path file |> Path.relative
     >>| (fun relative ->
         let handle = File.Handle.create relative in
         Analysis.Preprocessing.preprocess source
         |> AstSharedMemory.add_source handle;
         handle :: handles))
    |> Option.value ~default:handles
  in
  List.fold ~init:[] ~f:parse_source files


let parse_sources_list
    ~configuration:({ Configuration.source_root; project_root; _ } as configuration)
    ~scheduler
    ~files =
  AstSharedMemory.remove_paths (List.filter_map ~f:(File.handle ~root:source_root) files);
  let sources =
    if Scheduler.is_parallel scheduler then
      parse_parallel ~scheduler ~job:(parse_source_job ~configuration) ~files
    else
      parse_source_job ~configuration ~files
  in
  let strict_coverage, declare_coverage =
    List.fold
      ~init:(0, 0)
      ~f:(fun (prev_strict, prev_declare) handle ->
          match AstSharedMemory.get_source handle with
          | Some { Source.metadata = { Source.Metadata.strict; declare; _ }; _ } ->
              (
                prev_strict + (if strict then 1 else 0),
                prev_declare + (if declare then 1 else 0)
              )
          | None -> (prev_strict, prev_declare)
        )
      sources
  in
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:source_root
    |> Option.value ~default:(Path.absolute source_root)
  in
  Statistics.coverage
    ~coverage:[
      "strict_coverage", strict_coverage;
      "declare_coverage", declare_coverage;
      "default_coverage", List.length files - strict_coverage - declare_coverage;
      "source_files", List.length files;
    ]
    ~normals:[
      "file_name", path_to_files;
    ]
    ~configuration
    ();

  let not_parsed = (List.length files) - (List.length sources) in
  if not_parsed > 0 then
    Log.info "Unable to parse %d paths" not_parsed;
  (sources, (strict_coverage, declare_coverage))


let parse_sources scheduler ~configuration:({ Configuration.source_root; _ } as configuration) =
  let timer = Timer.start () in
  let paths =
    let is_python path = String.suffix path 3 = ".py" in
    let has_stub path =
      Sys.is_file ((Path.absolute path) ^ "i") = `Yes
    in
    File.list ~filter:is_python ~root:source_root
    |> List.filter ~f:(fun path -> not (has_stub path))
  in
  Log.info "Parsing %d sources in `%a`..." (List.length paths) Path.pp source_root;
  let (handles, _) =
    parse_sources_list ~configuration ~scheduler ~files:(List.map ~f:File.create paths)
  in
  Statistics.performance ~name:"sources parsed" ~timer ~configuration ();
  handles
