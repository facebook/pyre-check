(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ExitStatus = struct
  type t =
    | Ok
    | PyreError
  [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
    | PyreError -> 1
end

module QueryConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    query: string;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let query = json |> string_member "query" ~default:"" in
          Result.Ok { base; query }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)
end

let run_query configuration_file =
  let exit_status =
    match CommandStartup.read_and_parse_json configuration_file ~f:QueryConfiguration.of_yojson with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           QueryConfiguration.base =
             {
               CommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as query_configuration) ->
        CommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();
        let () =
          Printf.printf
            "Done setting up global states, with configuration value: %s"
            (Sexp.to_string (QueryConfiguration.sexp_of_t query_configuration))
        in
        ExitStatus.Ok
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  Printexc.record_backtrace true;
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs a full check without a server"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_query filename))
