(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Parsing: the core logic for converting a ModulePath to a parsed Source.t.
 *
 * The actual low-level parse is delegated to a parser implementation (e.g. cpython),
 * but this module handles metadata and representing errors.
 *)

open Core
open Ast

module LoadResult = struct
  module Code = struct
    type t = string [@@deriving sexp, compare, hash]
  end

  module Error = struct
    type t = string [@@deriving sexp, compare, hash]
  end

  type t = (Code.t, Error.t) Result.t [@@deriving sexp, compare, hash]
end

module ParseResult = struct
  module Error = struct
    type t = {
      module_path: Ast.ModulePath.t;
      location: Ast.Location.t;
      is_suppressed: bool;
      message: string;
    }
    [@@deriving sexp, compare, hash]
  end

  type t = (Ast.Source.t, Error.t) Result.t [@@deriving sexp, compare, hash]
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
  ParseResult.Error.{ location; message; is_suppressed; module_path }


let parse_raw_code_with_cpython
    ~configuration:({ Configuration.Analysis.enable_type_comments; _ } as configuration)
    ({ ModulePath.qualifier; _ } as module_path)
    raw_code
  =
  let parse context =
    let typecheck_flags = Source.TypecheckFlags.parse ~qualifier (String.split raw_code ~on:'\n') in
    match
      PyreCPythonParser.parse_module ~enable_type_comment:enable_type_comments ~context raw_code
    with
    | Ok statements -> Ok (create_source ~typecheck_flags ~module_path statements)
    | Error { PyreCPythonParser.Error.line; column; end_line; end_column; message } ->
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
  PyreCPythonParser.with_context parse


(* Parse the results of loading code, if no error. If there's any error (which could
 * happen either in parsing or upstream if we weren't able to load the code) represent
 * the error as a ParseError.t *)
let parse_result_of_load_result ~controls ~post_process module_path code_result =
  let configuration = EnvironmentControls.configuration controls in
  let post_process_source source =
    let {
      Configuration.Analysis.python_version = { Configuration.PythonVersion.major; minor; micro };
      system_platform;
      _;
    }
      =
      configuration
    in
    let sys_platform = Option.value system_platform ~default:"linux" in
    Preprocessing.replace_version_specific_code
      ~major_version:major
      ~minor_version:minor
      ~micro_version:micro
      source
    |> Preprocessing.replace_platform_specific_code ~sys_platform
    |> Preprocessing.preprocess_before_wildcards
  in
  match code_result with
  | Ok raw_code ->
      let result = parse_raw_code_with_cpython ~configuration module_path raw_code in
      if post_process then
        Result.map ~f:post_process_source result
      else
        result
  | Error load_error ->
      Error
        ParseResult.Error.
          {
            location =
              {
                Location.start = { Location.line = 1; column = 1 };
                stop = { Location.line = 1; column = 1 };
              };
            message = load_error;
            is_suppressed = false;
            module_path;
          }
