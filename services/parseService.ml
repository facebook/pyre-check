(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Ast
open Pyre


let parse_path_to_source file =
  File.path file |> Path.relative
  >>= fun path ->
  File.lines file
  >>= fun lines ->
  let metadata = Source.Metadata.parse lines in
  try
    let statements = PythonParse.parse ~path lines in
    Some (
      Source.create
        ~docstring:(Statement.extract_docstring statements)
        ~metadata
        ~path
        ~qualifier:(Source.qualifier ~path)
        statements)
  with
  | PythonParse.Error error ->
      Log.error "%s" error;
      None

let parse_parallel parse_job paths service =
  Service.map_reduce
    service
    ~init:[]
    ~map:(fun _ paths -> parse_job paths)
    ~reduce:(fun new_sources parsed_sources -> parsed_sources @ new_sources)
    paths


(** Adds the preprocessing for stubs *)
let parse_stub_job paths =
  let parse_stub handles path =
    (File.create path
     |> parse_path_to_source
     >>= fun source ->
     Path.relative path
     >>| fun relative ->
     let preprocess source =
       let source =
         (* Drop version from qualifier. *)
         let qualifier =
           let is_digit access =
             try
               Instantiated.Access.show [access]
               |> Int.of_string
               |> ignore;
               true
             with _ ->
               false
           in
           match source.Source.qualifier with
           | minor :: major :: tail when is_digit minor && is_digit major ->
               tail
           | qualifier ->
               List.tl_exn qualifier
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
  List.fold ~init:[] ~f:parse_stub paths


let parse_stubs service ~roots =
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
  let paths = List.fold ~init:[] ~f:paths roots in

  let handles =
    if Service.is_parallel service then
      parse_parallel parse_stub_job paths service
    else
      parse_stub_job paths
  in

  let not_parsed = (List.length paths) - (List.length handles) in
  if not_parsed > 0 then
    Log.info "Unable to parse %d stubs" not_parsed;
  Log.log ~section:`Performance "Stubs parsed in %fs" (Timer.stop timer);

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


let parse_sources_list service files =
  AstSharedMemory.remove_paths (List.filter_map ~f:File.handle files);
  let sources =
    if Service.is_parallel service then
      parse_parallel parse_source_job files service
    else
      parse_source_job files
  in
  let not_parsed = (List.length files) - (List.length sources) in
  if not_parsed > 0 then
    Log.info "Unable to parse %d paths" not_parsed;
  sources


let parse_sources service ~root =
  let timer = Timer.start () in
  let paths =
    let is_python path =
      String.suffix path 3 = ".py" in
    File.list ~filter:is_python ~root:root in
  Log.info "Parsing %d sources in `%a`..." (List.length paths) Path.pp root;
  let handles = parse_sources_list service (List.map ~f:File.create paths) in
  let strict_number =
    List.count
      handles
      ~f:(fun handle ->
          match AstSharedMemory.get_source handle with
          | Some { Source.metadata; _ } -> metadata.Source.Metadata.strict
          | None -> false
        )
  in
  Statistics.coverage
    ~flush:true
    ~percentages:["strict_coverage", strict_number]
    ~labels:["root", root |> Path.show;]
  |> ignore;
  Log.log ~section:`Performance "Sources parsed in %fs" (Timer.stop timer);
  handles
