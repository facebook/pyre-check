(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Parsing: the core logic for converting a ModulePath to a parsed Source.t.
 *
 * The actual low-level parse is delegated to a parser implementation (e.g. Errpy),
 * but this module handles metadata and representing errors.
 *
 * The only functional dependency of this logic is a code_of_module_path callback.
 *)

open Core
open Ast

module ParserError = struct
  type t = {
    module_path: Ast.ModulePath.t;
    location: Ast.Location.t;
    is_suppressed: bool;
    message: string;
  }
  [@@deriving sexp, compare, hash]
end

let create_source ~typecheck_flags ~module_path statements =
  Source.create_from_module_path
    ~collect_format_strings_with_ignores:Visit.collect_format_strings_with_ignores
    ~typecheck_flags
    ~module_path
    statements


let create_parse_error
    ~configuration
    ~typecheck_flags
    ~module_path
    ~line
    ~column
    ~end_line
    ~end_column
    ~message
    ()
  =
  let is_suppressed =
    let { Source.TypecheckFlags.local_mode; ignore_codes; _ } = typecheck_flags in
    match Source.mode ~configuration ~local_mode with
    | Source.Declare -> true
    | _ ->
        (* NOTE: The number needs to be updated when the error code changes. *)
        List.exists ignore_codes ~f:(Int.equal 404)
  in
  let location =
    (* CPython set line/column number to -1 in some exceptional cases. *)
    let replace_invalid_position number = if number <= 0 then 1 else number in
    let start =
      { Location.line = replace_invalid_position line; column = replace_invalid_position column }
    in
    let stop =
      (* Work around CPython bug where the end location sometimes precedes start location. *)
      if [%compare: int * int] (line, column) (end_line, end_column) > 0 then
        start
      else
        {
          Location.line = replace_invalid_position end_line;
          column = replace_invalid_position end_column;
        }
    in
    { Location.start; stop }
  in
  ParserError.{ location; message; is_suppressed; module_path }


let parse_raw_code_with_cpython
    ~configuration:({ Configuration.Analysis.enable_type_comments; _ } as configuration)
    ({ ModulePath.qualifier; _ } as module_path)
    raw_code
  =
  let parse context =
    let typecheck_flags = Source.TypecheckFlags.parse ~qualifier (String.split raw_code ~on:'\n') in
    match
      PyreNewParser.parse_module ~enable_type_comment:enable_type_comments ~context raw_code
    with
    | Ok statements -> Ok (create_source ~typecheck_flags ~module_path statements)
    | Error { PyreNewParser.Error.line; column; end_line; end_column; message } ->
        Error
          (create_parse_error
             ~configuration
             ~typecheck_flags
             ~module_path
             ~line
             ~column
             ~end_line
             ~end_column
             ~message
             ())
  in
  PyreNewParser.with_context parse


let parse_raw_code_with_errpy ~configuration ({ ModulePath.qualifier; _ } as module_path) raw_code =
  let timer = Timer.start () in
  let log_errpy_ok ~recovered_count =
    let integers = ["recovered_count", recovered_count] in
    let normals =
      match recovered_count with
      | 0 -> []
      | _ -> (
          match Int.equal (Random.int 100) 0 with
          | false -> []
          | true ->
              (*so as to avoid a torrent of data we only log 1/100 of the sources where there is
                error recovery for the purposes of error recovery quality management *)
              ["raw_code", raw_code])
    in
    Statistics.errpy_call ~flush:false ~name:"ok" ~timer ~integers ~normals ()
  in
  let log_errpy_error ~error_string =
    Statistics.errpy_call
      ~flush:true
      ~name:"error"
      ~timer
      ~integers:[]
      ~normals:["raw_code", raw_code; "error", error_string]
      ()
  in
  let typecheck_flags = Source.TypecheckFlags.parse ~qualifier (String.split raw_code ~on:'\n') in
  match PyreErrpyParser.parse_module raw_code with
  | Ok statements ->
      log_errpy_ok ~recovered_count:0;
      Ok (create_source ~typecheck_flags ~module_path statements)
  | Error parserError -> (
      match parserError with
      | Recoverable recoverable ->
          log_errpy_ok ~recovered_count:(List.length recoverable.errors);
          Ok (create_source ~typecheck_flags ~module_path recoverable.recovered_ast)
      | Unrecoverable error_string ->
          log_errpy_error ~error_string;
          Error
            (create_parse_error
               ~configuration
               ~typecheck_flags
               ~module_path
               ~line:1
               ~column:1
               ~end_line:1
               ~end_column:1
               ~message:error_string
               ()))


let load_and_parse ~controls ~code_of_module_path module_path =
  let configuration = EnvironmentControls.configuration controls in
  let parse_raw_code =
    match configuration with
    | { use_errpy_parser = false; _ } -> parse_raw_code_with_cpython
    | _ -> parse_raw_code_with_errpy
  in
  let post_process_source source =
    let EnvironmentControls.PythonVersionInfo.{ major_version; minor_version; micro_version } =
      EnvironmentControls.python_version_info controls
    in
    Preprocessing.replace_version_specific_code ~major_version ~minor_version ~micro_version source
    |> Preprocessing.preprocess_before_wildcards
  in
  match code_of_module_path module_path with
  | Ok raw_code ->
      parse_raw_code ~configuration module_path raw_code |> Result.map ~f:post_process_source
  | Error message ->
      Error
        ParserError.
          {
            location =
              {
                Location.start = { Location.line = 1; column = 1 };
                stop = { Location.line = 1; column = 1 };
              };
            message;
            is_suppressed = false;
            module_path;
          }
