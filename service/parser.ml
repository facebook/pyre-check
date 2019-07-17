(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open PyreParser

type 'success parse_result =
  | Success of 'success
  | SyntaxError of File.Handle.t
  | SystemError of File.Handle.t

let parse_source ~configuration ?(show_parser_errors = true) file =
  let parse_lines ~handle lines =
    let qualifier = Source.qualifier ~handle in
    let metadata = Source.Metadata.parse ~qualifier lines in
    try
      let statements = Parser.parse ~handle lines in
      let hash = [%hash: string list] lines in
      Success
        (Source.create
           ~docstring:(Statement.extract_docstring statements)
           ~hash
           ~metadata
           ~handle
           ~qualifier
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
  File.lines file >>| parse_lines ~handle |> Option.value ~default:(SystemError handle)


module FixpointResult = struct
  type t = {
    parsed: File.Handle.t parse_result list;
    not_parsed: File.t list
  }

  let merge
      { parsed = left_parsed; not_parsed = left_not_parsed }
      { parsed = right_parsed; not_parsed = right_not_parsed }
    =
    { parsed = left_parsed @ right_parsed; not_parsed = left_not_parsed @ right_not_parsed }
end

let parse_sources_job ~preprocessing_state ~show_parser_errors ~force ~configuration ~files =
  let parse ({ FixpointResult.parsed; not_parsed } as result) file =
    let use_parsed_source source =
      let source =
        match preprocessing_state with
        | Some state -> ProjectSpecificPreprocessing.preprocess ~state source
        | None -> source
      in
      let store_result ~preprocessed ~file =
        let add_module_from_source ({ Source.qualifier; _ } as source) =
          Module.create source
          |> fun ast_module -> Ast.SharedMemory.Modules.add ~qualifier ~ast_module
        in
        add_module_from_source preprocessed;
        let handle = File.handle ~configuration file in
        let qualifier = Source.qualifier ~handle in
        Ast.SharedMemory.Handles.add qualifier ~handle:(File.Handle.show handle);
        Ast.SharedMemory.Sources.add (Plugin.apply_to_ast preprocessed);
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
        | None -> { result with not_parsed = file :: not_parsed }
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
  system_error: File.Handle.t list
}

let parse_sources ~configuration ~scheduler ~preprocessing_state ~files =
  let rec fixpoint ?(force = false) ({ FixpointResult.parsed; not_parsed } as input_state) =
    let { FixpointResult.parsed = new_parsed; not_parsed = new_not_parsed } =
      Scheduler.map_reduce
        scheduler
        ~configuration
        ~initial:{ FixpointResult.parsed = []; not_parsed = [] }
        ~map:(fun _ files ->
          parse_sources_job
            ~show_parser_errors:(List.length parsed = 0)
            ~preprocessing_state
            ~force
            ~configuration
            ~files)
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
  let result = fixpoint { parsed = []; not_parsed = files } in
  let () =
    let get_qualifier file =
      File.handle ~configuration file |> fun handle -> Source.qualifier ~handle
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
    let trace list = List.map list ~f:File.Handle.show |> String.concat ~sep:";" in
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
  let { parsed; syntax_error; system_error } =
    let preprocessing_state =
      ProjectSpecificPreprocessing.initial (ModuleTracker.mem module_tracker)
    in
    let files = ModuleTracker.paths module_tracker |> List.map ~f:File.create in
    parse_sources ~configuration ~scheduler ~preprocessing_state:(Some preprocessing_state) ~files
  in
  log_parse_errors ~syntax_error ~system_error;
  Statistics.performance ~name:"sources parsed" ~timer ();
  parsed
