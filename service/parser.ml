(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser


type 'success parse_result =
  | Success of 'success
  | SyntaxError of File.Handle.t
  | SystemError of File.Handle.t


let parse_source ~configuration ?(show_parser_errors = true) file =
  let parse_lines ~handle lines =
    let metadata = Source.Metadata.parse (File.Handle.show handle) lines in
    try
      let statements = Parser.parse ~handle lines in
      let hash = [%hash: string list] lines in
      Success (
        Source.create
          ~docstring:(Statement.extract_docstring statements)
          ~hash
          ~metadata
          ~handle
          ~qualifier:(Source.qualifier ~handle)
          statements)
    with
    | Parser.Error error ->
        if show_parser_errors then
          Log.log ~section:`Parser "%s" error;
        SyntaxError handle
    | Failure error ->
        Log.error "%s" error;
        SystemError handle
  in
  File.handle ~configuration file
  |> fun handle ->
  Path.readlink (File.path file)
  >>| (fun target -> Ast.SharedMemory.SymlinksToPaths.add target (File.path file))
  |> ignore;
  File.lines file
  >>| parse_lines ~handle
  |> Option.value ~default:(SystemError handle)


module FixpointResult = struct
  type t = {
    parsed: File.Handle.t parse_result list;
    not_parsed: File.t list;
  }

  let merge
      { parsed = left_parsed; not_parsed = left_not_parsed }
      { parsed = right_parsed; not_parsed = right_not_parsed } =
    {
      parsed = left_parsed @ right_parsed;
      not_parsed = left_not_parsed @ right_not_parsed;
    }
end


let parse_sources_job ~show_parser_errors ~force ~configuration ~files =
  let parse ({ FixpointResult.parsed; not_parsed } as result) file =
    let use_parsed_source source =
      let store_result ~preprocessed ~file =
        let add_module_from_source
            {
              Source.qualifier;
              handle;
              statements;
              metadata = { Source.Metadata.local_mode; _ };
              _;
            } =
          Module.create
            ~qualifier
            ~local_mode
            ~handle
            ~stub:(File.Handle.is_stub handle)
            statements
          |> fun ast_module -> Ast.SharedMemory.Modules.add ~qualifier ~ast_module
        in

        add_module_from_source preprocessed;
        let handle = File.handle ~configuration file in
        Ast.SharedMemory.Handles.add_handle_hash ~handle:(File.Handle.show handle);
        Plugin.apply_to_ast preprocessed
        |> Ast.SharedMemory.Sources.add handle;
        handle
      in

      if force then
        let handle =
          Analysis.Preprocessing.preprocess source
          |> fun preprocessed -> store_result ~preprocessed ~file
        in
        { result with parsed = Success handle :: parsed }
      else
        match Analysis.Preprocessing.try_preprocess source with
        | Some preprocessed ->
            let handle = store_result ~preprocessed ~file in
            { result with parsed = Success handle :: parsed }
        | None ->
            { result with not_parsed = file :: not_parsed }
    in

    parse_source ~configuration ~show_parser_errors file
    |> fun parsed_source ->
    match parsed_source with
    | Success parsed -> use_parsed_source parsed
    | SyntaxError error -> { result with parsed = SyntaxError error :: parsed }
    | SystemError error -> { result with parsed = SystemError error :: parsed }
  in
  List.fold ~init:{ FixpointResult.parsed = []; not_parsed = [] } ~f:parse files


type parse_sources_result = {
  parsed: File.Handle.t list;
  syntax_error: File.Handle.t list;
  system_error: File.Handle.t list;
}


let parse_sources ~configuration ~scheduler ~files =
  let rec fixpoint ?(force = false) ({ FixpointResult.parsed; not_parsed } as input_state) =
    let { FixpointResult.parsed = new_parsed; not_parsed = new_not_parsed } =
      Scheduler.map_reduce
        scheduler
        ~configuration
        ~initial:{ FixpointResult.parsed = []; not_parsed = [] }
        ~map:(fun _ files ->
            parse_sources_job
              ~show_parser_errors:((List.length parsed) = 0)
              ~force
              ~configuration
              ~files)
        ~reduce:FixpointResult.merge
        ~inputs:not_parsed
        ()
    in

    if List.is_empty new_not_parsed then
      (* All done. *)
      parsed @ new_parsed
    else if List.is_empty new_parsed then
      (* No progress was made, force the parse ignoring all temporary errors. *)
      fixpoint ~force:true input_state
    else
      (* We made some progress, continue with the fixpoint. *)
      fixpoint { parsed = parsed @ new_parsed; not_parsed = new_not_parsed }
  in
  let result = fixpoint { parsed = []; not_parsed = files } in
  let () =
    let get_qualifier file =
      File.handle ~configuration file
      |> (fun handle -> Source.qualifier ~handle)
    in
    List.map files ~f:get_qualifier
    |> fun qualifiers -> Ast.SharedMemory.Modules.remove ~qualifiers
  in
  let categorize ({ parsed; syntax_error; system_error } as result) parse_result =
    match parse_result with
    | Success handle -> { result with parsed = handle :: parsed }
    | SyntaxError handle -> { result with syntax_error = handle :: syntax_error }
    | SystemError handle -> { result with system_error = handle :: system_error }
  in
  List.fold result ~init:{ parsed = []; syntax_error = []; system_error = [] } ~f:categorize


let log_parse_errors ~syntax_error ~system_error ~description =
  let syntax_errors = List.length syntax_error in
  let system_errors = List.length system_error in
  let count = syntax_errors + system_errors in
  if count > 0 then
    let hint =
      if syntax_errors > 0 && not (Log.is_enabled `Parser) then
        Format.asprintf
          " Run `pyre --show-parse-errors %s` for more details%s."
          (try (Array.nget Sys.argv 1) with _ -> "restart")
          (if system_errors > 0 then " on the syntax errors" else "")
      else
        ""
    in
    let details =
      let to_string count description =
        Format.sprintf "%d %s%s"
          count
          description
          (if count == 1 then "" else "s")
      in
      if syntax_errors > 0 && system_errors > 0 then
        Format.sprintf
          ": %s, %s"
          (to_string syntax_errors "syntax error")
          (to_string system_errors "system error")
      else if syntax_errors > 0 then
        " due to syntax errors"
      else
        " due to system errors"
    in
    Log.warning
      "Could not parse %d %s%s%s!%s"
      count
      description
      (if count > 1 then "s" else "")
      details
      hint;
    let trace list =
      List.map list ~f:File.Handle.show
      |> String.concat ~sep:";"
    in
    Statistics.event
      ~flush:true
      ~name:"parse errors"
      ~integers:[
        "syntax errors", syntax_errors;
        "system errors", system_errors;
      ]
      ~normals:[
        "description", description;
        "syntax errors trace", trace syntax_error;
        "system errors trace", trace system_error;
      ]
      ()


let find_stubs
    ~configuration:(
    { Configuration.Analysis.local_root; typeshed; search_path; excludes; _ } as configuration) =
  let paths =
    let stubs =
      let typeshed_directories =
        let list_subdirectories typeshed_path =
          let root = Path.absolute typeshed_path in
          if Core.Sys.is_directory root = `Yes then
            match Core.Sys.ls_dir root with
            | entries ->
                let select_directories sofar path =
                  if Core.Sys.is_directory (root ^/ path) = `Yes &&
                     path <> "tests" &&
                     not (String.is_prefix path ~prefix:".")
                  then
                    (Path.create_relative ~root:typeshed_path ~relative:path) :: sofar
                  else
                    sofar
                in
                List.fold ~init:[] ~f:select_directories entries
            | exception Sys_error _ ->
                Log.error "Could not list typeshed directory: `%s`" root;
                []
          else
            begin
              Log.info "Not a typeshed directory: `%s`" root;
              []
            end
        in
        Option.value_map ~default:[] ~f:(fun path -> list_subdirectories path) typeshed
      in
      let stubs root =
        Log.info "Finding type stubs in `%a`..." Path.pp root;
        let directory_filter path =
          let is_python_2_directory path =
            String.is_suffix ~suffix:"/2" path ||
            String.is_suffix ~suffix:"/2.7" path
          in
          not (is_python_2_directory path) &&
          not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0))
        in
        let file_filter path =
          String.is_suffix path ~suffix:".pyi" &&
          not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0))
        in
        Path.list ~file_filter ~directory_filter ~root ()
      in
      let search_path = List.map search_path ~f:Path.SearchPath.to_path in
      List.map ~f:stubs (local_root :: (search_path @ typeshed_directories))
    in
    let modules =
      let modules root =
        Log.info "Finding external sources in `%a`..." Path.pp root;
        let directory_filter path =
          not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0))
        in
        let file_filter path =
          String.is_suffix ~suffix:".py" path &&
          not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0))
        in
        Path.list ~file_filter ~directory_filter ~root ()
      in
      search_path
      |> List.map ~f:Path.SearchPath.to_path
      |> List.map ~f:modules
    in

    stubs @ modules
  in
  let _, paths =
    (* If two stub directories contain the same stub, prefer the one that
       appears earlier in the search path. *)
    let filter_interfering_stubs (qualifiers, all_paths) paths =
      let add (qualifiers, all_paths) path =
        let handle =
          File.create path
          |> File.handle ~configuration
        in
        let qualifier = Ast.Source.qualifier ~handle in
        if Set.mem qualifiers qualifier then
          qualifiers, all_paths
        else
          Set.add qualifiers qualifier, path :: all_paths
      in
      List.fold ~f:add ~init:(qualifiers, all_paths) paths
    in
    List.fold ~f:filter_interfering_stubs ~init:(Access.Set.empty, []) paths
  in
  paths


let find_sources ?(filter = fun _ -> true) { Configuration.Analysis.local_root; excludes; _ } =
  let directory_filter path =
    not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0))
  in
  let file_filter path =
    String.is_suffix ~suffix:".py" path &&
    not (String.is_substring ~substring: ".pyre/resource_cache" path) &&
    not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0)) &&
    filter path
  in
  Path.list ~file_filter ~directory_filter ~root:local_root ()


type result = {
  stubs: File.Handle.t list;
  sources: File.Handle.t list;
}


let parse_all scheduler ~configuration:({ Configuration.Analysis.local_root; _ } as configuration) =
  let stubs =
    let timer = Timer.start () in
    let stub_paths = find_stubs ~configuration in
    Log.info "Parsing %d stubs and external sources..." (List.length stub_paths);
    let { parsed; syntax_error; system_error }  =
      parse_sources ~configuration ~scheduler ~files:(List.map ~f:File.create stub_paths)
    in
    log_parse_errors ~syntax_error ~system_error ~description:"external file";
    Statistics.performance ~name:"stubs parsed" ~timer ();
    parsed
  in
  let known_stubs =
    let add_to_known_stubs sofar handle =
      match Ast.SharedMemory.Sources.get handle with
      | Some { Ast.Source.qualifier; handle; _ } ->
          if Set.mem sofar qualifier then
            Statistics.event
              ~name:"interfering stub"
              ~normals:["handle", File.Handle.show handle]
              ();
          Set.add sofar qualifier
      | _ ->
          sofar
    in
    List.fold stubs ~init:Access.Set.empty ~f:add_to_known_stubs
  in
  let sources =
    let filter path =
      let relative =
        Path.get_relative_to_root
          ~root:local_root
          (* We want to filter based on the path of the symlink instead of the path the
             symlink points to. *)
          ~path:(Path.create_absolute ~follow_symbolic_links:false path)
      in
      match relative with
      | Some handle ->
          handle = "__init__.py" ||  (* Analyze top-level `__init__.py`. *)
          not (Set.mem known_stubs (Source.qualifier ~handle:(File.Handle.create handle)))
      | _ ->
          true
    in
    let timer = Timer.start () in
    let paths = find_sources configuration ~filter in
    Log.info "Parsing %d sources in `%a`..." (List.length paths) Path.pp local_root;
    let { parsed; syntax_error; system_error }  =
      parse_sources ~configuration ~scheduler ~files:(List.map ~f:File.create paths)
    in
    log_parse_errors ~syntax_error ~system_error ~description:"file";
    Statistics.performance ~name:"sources parsed" ~timer ();
    parsed
  in
  { stubs; sources }
