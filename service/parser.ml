(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre
open PyreParser

type 'success parse_result =
  | Success of 'success
  | SyntaxError of SourcePath.t
  | SystemError of SourcePath.t

let parse_source
    ?(show_parser_errors = true)
    ~configuration
    ({ SourcePath.relative; qualifier; _ } as source_path)
  =
  let parse_lines lines =
    let metadata = Source.Metadata.parse ~qualifier lines in
    try
      let statements = Parser.parse ~relative lines in
      let hash = [%hash: string list] lines in
      Success
        (Source.create_from_source_path
           ~docstring:(Statement.extract_docstring statements)
           ~metadata
           ~hash
           ~source_path
           statements)
    with
    | Parser.Error error ->
        if show_parser_errors then
          Log.log ~section:`Parser "%s" error;
        SyntaxError source_path
    | Failure error ->
        Log.error "%s" error;
        SystemError source_path
  in
  SourcePath.full_path ~configuration source_path
  |> File.create
  |> File.lines
  >>| parse_lines
  |> Option.value ~default:(SystemError source_path)


module FixpointResult = struct
  type t = {
    parsed: SourcePath.t parse_result list;
    not_parsed: SourcePath.t list;
  }

  let merge
      { parsed = left_parsed; not_parsed = left_not_parsed }
      { parsed = right_parsed; not_parsed = right_not_parsed }
    =
    { parsed = left_parsed @ right_parsed; not_parsed = left_not_parsed @ right_not_parsed }
end

let parse_sources_job
    ~configuration
    ~preprocessing_state
    ~ast_environment
    ~show_parser_errors
    ~force
    source_paths
  =
  let parse
      ({ FixpointResult.parsed; not_parsed } as result)
      ({ SourcePath.relative; qualifier; _ } as source_path)
    =
    let use_parsed_source source =
      let source =
        match preprocessing_state with
        | Some state -> ProjectSpecificPreprocessing.preprocess ~state source
        | None -> source
      in
      let store_result preprocessed =
        let add_module_from_source ({ Source.qualifier; _ } as source) =
          Module.create source
          |> fun ast_module ->
          (* TODO (T47860871): Deprecate Ast.SharedMemory.Modules *)
          Ast.SharedMemory.Modules.add ~qualifier ~ast_module
        in
        add_module_from_source preprocessed;

        (* TODO (T47860888): Deprecate Ast.SharedMemory.Handles *)
        Ast.SharedMemory.Handles.add qualifier ~handle:relative;
        AstEnvironment.add_source ast_environment (Plugin.apply_to_ast preprocessed)
      in
      match force with
      | true ->
          store_result (Preprocessing.preprocess source);
          { result with parsed = Success source_path :: parsed }
      | false -> (
        match Preprocessing.try_preprocess source with
        | Some preprocessed ->
            store_result preprocessed;
            { result with parsed = Success source_path :: parsed }
        | None -> { result with not_parsed = source_path :: not_parsed } )
    in
    parse_source ~configuration ~show_parser_errors source_path
    |> fun parsed_source ->
    match parsed_source with
    | Success parsed -> use_parsed_source parsed
    | SyntaxError error -> { result with parsed = SyntaxError error :: parsed }
    | SystemError error -> { result with parsed = SystemError error :: parsed }
  in
  List.fold ~init:{ FixpointResult.parsed = []; not_parsed = [] } ~f:parse source_paths


type parse_sources_result = {
  parsed: SourcePath.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

let parse_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment source_paths =
  let rec fixpoint ?(force = false) ({ FixpointResult.parsed; not_parsed } as input_state) =
    let { FixpointResult.parsed = new_parsed; not_parsed = new_not_parsed } =
      Scheduler.map_reduce
        scheduler
        ~configuration
        ~initial:{ FixpointResult.parsed = []; not_parsed = [] }
        ~map:(fun _ source_paths ->
          parse_sources_job
            ~configuration
            ~show_parser_errors:(List.length parsed = 0)
            ~preprocessing_state
            ~ast_environment
            ~force
            source_paths)
        ~reduce:FixpointResult.merge
        ~inputs:not_parsed
        ()
    in
    if List.is_empty new_not_parsed then (* All done. *)
      parsed @ new_parsed
    else if List.is_empty new_parsed then
      (* No progress was made, force the parse ignoring all temporary errors. *)
      fixpoint ~force:true input_state
    else (* We made some progress, continue with the fixpoint. *)
      fixpoint { parsed = parsed @ new_parsed; not_parsed = new_not_parsed }
  in
  let result = fixpoint { parsed = []; not_parsed = source_paths } in
  let () =
    List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
    |> fun qualifiers -> Ast.SharedMemory.Modules.remove ~qualifiers
  in
  let categorize ({ parsed; syntax_error; system_error } as result) parse_result =
    match parse_result with
    | Success source_path -> { result with parsed = source_path :: parsed }
    | SyntaxError source_path -> { result with syntax_error = source_path :: syntax_error }
    | SystemError source_path -> { result with system_error = source_path :: system_error }
  in
  List.fold result ~init:{ parsed = []; syntax_error = []; system_error = [] } ~f:categorize


let log_parse_errors ~syntax_error ~system_error =
  let syntax_errors = List.length syntax_error in
  let system_errors = List.length system_error in
  let count = syntax_errors + system_errors in
  if count > 0 then (
    let hint =
      if syntax_errors > 0 && not (Log.is_enabled `Parser) then
        Format.asprintf
          " Run `pyre %s` without `--hide-parse-errors` for more details%s."
          ( try Array.nget Sys.argv 1 with
          | _ -> "restart" )
          (if system_errors > 0 then " on the syntax errors" else "")
      else
        ""
    in
    let details =
      let to_string count description =
        Format.sprintf "%d %s%s" count description (if count == 1 then "" else "s")
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
    Log.warning "Could not parse %d file%s%s!%s" count (if count > 1 then "s" else "") details hint;
    let trace list =
      List.map list ~f:(fun { SourcePath.relative; _ } -> relative) |> String.concat ~sep:";"
    in
    Statistics.event
      ~flush:true
      ~name:"parse errors"
      ~integers:["syntax errors", syntax_errors; "system errors", system_errors]
      ~normals:
        ["syntax errors trace", trace syntax_error; "system errors trace", trace system_error]
      () )


let parse_all ~scheduler ~configuration module_tracker =
  let timer = Timer.start () in
  Log.info "Parsing %d stubs and sources..." (ModuleTracker.length module_tracker);
  let ast_environment = AstEnvironment.create module_tracker in
  let { parsed; syntax_error; system_error } =
    let preprocessing_state =
      ProjectSpecificPreprocessing.initial (ModuleTracker.mem module_tracker)
    in
    ModuleTracker.source_paths module_tracker
    |> parse_sources
         ~configuration
         ~scheduler
         ~preprocessing_state:(Some preprocessing_state)
         ~ast_environment
  in
  log_parse_errors ~syntax_error ~system_error;
  Statistics.performance ~name:"sources parsed" ~timer ();
  parsed, ast_environment
