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
  | Parser.Error error
  | Failure error ->
      Log.error "%s" error;
      None

let parse_parallel parse_job paths scheduler =
  Scheduler.map_reduce
    scheduler
    ~init:[]
    ~map:(fun _ paths -> parse_job paths)
    ~reduce:(fun new_sources parsed_sources -> parsed_sources @ new_sources)
    paths


(** Adds the preprocessing for stubs *)
let parse_stub_job files =
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



let parse_stubs_list scheduler files =
  let handles =
    if Scheduler.is_parallel scheduler then
      parse_parallel parse_stub_job files scheduler
    else
      parse_stub_job files
  in
  handles

let parse_stubs
    scheduler
    ~configuration:({ Configuration.source_root; stub_roots; _ } as configuration) =
  let timer = Timer.start () in
  let paths sofar root =
    Log.info "Parsing type stubs in `%a`..." Path.pp root;
    let is_stub path =
      let is_python_2 path =
        String.is_substring ~substring:"/2/" path ||
        String.is_substring ~substring:"/2.7/" path in
      String.suffix path 4 = ".pyi" && not (is_python_2 path) in
    sofar @ File.list ~filter:is_stub ~root
  in
  let paths = List.fold ~init:[] ~f:paths (source_root :: stub_roots) in
  let handles = parse_stubs_list scheduler (List.map ~f:File.create paths) in
  Statistics.performance ~name:"stubs parsed" ~timer ~configuration ();
  let not_parsed = (List.length paths) - (List.length handles) in
  if not_parsed > 0 then
    Log.info "Unable to parse %d stubs" not_parsed;
  handles


(** Adds the preprocessing for sources *)
let parse_source_job files =
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
    scheduler
    files
    ~configuration:({ Configuration.source_root; project_root; _ } as configuration) =
  AstSharedMemory.remove_paths (List.filter_map ~f:(File.handle ~root:source_root) files);
  let sources =
    if Scheduler.is_parallel scheduler then
      parse_parallel parse_source_job files scheduler
    else
      parse_source_job files
  in
  let paths =
    let is_python path =
      String.suffix path 3 = ".py" in
    File.list ~filter:is_python ~root:source_root in
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
      "default_coverage", List.length paths - strict_coverage - declare_coverage;
      "source_files", List.length paths;
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
    let is_python path =
      String.suffix path 3 = ".py" in
    File.list ~filter:is_python ~root:source_root in
  Log.info "Parsing %d sources in `%a`..." (List.length paths) Path.pp source_root;
  let (handles, _) =
    parse_sources_list scheduler ~configuration (List.map ~f:File.create paths)
  in
  Statistics.performance ~name:"sources parsed" ~timer ~configuration ();
  handles
