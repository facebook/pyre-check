(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre
open PyreParser

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

let parse_source ~configuration ({ SourcePath.relative; qualifier; _ } as source_path) =
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
    | Parser.Error error -> SyntaxError error
    | Failure error -> SystemError error
  in
  let path = SourcePath.full_path ~configuration source_path in
  match File.lines (File.create path) with
  | Some lines -> parse_lines lines
  | None ->
      let message = Format.asprintf "Cannot open file %a" Path.pp path in
      SystemError message


module FixpointResult = struct
  type t = {
    processed: Source.t list;
    not_processed: Source.t list;
  }

  let merge
      { processed = left_processed; not_processed = left_not_processed }
      { processed = right_processed; not_processed = right_not_processed }
    =
    {
      processed = left_processed @ right_processed;
      not_processed = left_not_processed @ right_not_processed;
    }
end

let process_sources_job ~preprocessing_state ~ast_environment ~force sources =
  let process ({ FixpointResult.processed; not_processed } as result) source =
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

      let stored = Plugin.apply_to_ast preprocessed in
      AstEnvironment.add_source ast_environment stored;
      stored
    in
    match force with
    | true ->
        let source = store_result (Preprocessing.preprocess source) in
        { result with processed = source :: processed }
    | false -> (
      match Preprocessing.try_preprocess source with
      | Some preprocessed ->
          let source = store_result preprocessed in
          { result with processed = source :: processed }
      | None -> { result with not_processed = source :: not_processed } )
  in
  List.fold ~init:{ FixpointResult.processed = []; not_processed = [] } ~f:process sources


type parse_sources_result = {
  parsed: Source.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

let parse_raw_sources ~configuration ~scheduler source_paths =
  let results =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:[]
      ~map:(fun _ source_paths -> List.map source_paths ~f:(parse_source ~configuration))
      ~reduce:List.append
      ~inputs:source_paths
      ()
  in
  let categorize ({ parsed; syntax_error; system_error } as result) (source_path, parse_result) =
    match parse_result with
    | Success source -> { result with parsed = source :: parsed }
    | SyntaxError message ->
        Log.log ~section:`Parser "%s" message;
        { result with syntax_error = source_path :: syntax_error }
    | SystemError message ->
        Log.error "%s" message;
        { result with system_error = source_path :: system_error }
  in
  List.zip_exn source_paths results
  |> List.fold ~init:{ parsed = []; syntax_error = []; system_error = [] } ~f:categorize


let process_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment sources =
  let rec fixpoint ~force ({ FixpointResult.processed; not_processed } as input_state) =
    let { FixpointResult.processed = new_processed; not_processed = new_not_processed } =
      Scheduler.map_reduce
        scheduler
        ~configuration
        ~initial:{ FixpointResult.processed = []; not_processed = [] }
        ~map:(fun _ sources ->
          process_sources_job ~preprocessing_state ~ast_environment ~force sources)
        ~reduce:FixpointResult.merge
        ~inputs:not_processed
        ()
    in
    if List.is_empty new_not_processed then (* All done. *)
      processed @ new_processed
    else if List.is_empty new_processed then
      (* No progress was made, force the parse ignoring all temporary errors. *)
      fixpoint ~force:true input_state
    else (* We made some progress, continue with the fixpoint. *)
      fixpoint
        ~force:false
        { processed = processed @ new_processed; not_processed = new_not_processed }
  in
  let result = fixpoint ~force:false { FixpointResult.processed = []; not_processed = sources } in
  let () =
    List.map result ~f:(fun { Source.qualifier; _ } -> qualifier)
    |> fun qualifiers -> Ast.SharedMemory.Modules.remove ~qualifiers
  in
  result


let parse_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment source_paths =
  let { parsed; syntax_error; system_error } =
    parse_raw_sources ~configuration ~scheduler source_paths
  in
  let processed =
    process_sources ~configuration ~scheduler ~preprocessing_state ~ast_environment parsed
  in
  { parsed = processed; syntax_error; system_error }


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
