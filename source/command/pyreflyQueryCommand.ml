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
    | QueryParsingError of string
    | UnsupportedQuery of string
  [@@deriving sexp, compare, hash, show]

  let exit_code = function
    | Ok -> 0
    | QueryParsingError _ -> 1
    | UnsupportedQuery _ -> 2
end

let run_pyrefly_query ~pyrefly_results:_ ~query =
  match Server.Query.parse_request query with
  | Result.Error message -> ExitStatus.QueryParsingError message
  | Result.Ok (ModelQuery { path = _; query_name = _ }) -> ExitStatus.Ok
  | Result.Ok _ -> ExitStatus.UnsupportedQuery query


let run_command pyrefly_results query =
  let exit_status = run_pyrefly_query ~pyrefly_results ~query in
  let () =
    match exit_status with
    | ExitStatus.Ok -> ()
    | _ -> Log.error "Failed to execute query: `%a`" ExitStatus.pp exit_status
  in
  exit_status |> ExitStatus.exit_code |> exit


let doc = "Run a query using Pyrefly results"

let command () =
  let open Cmdliner in
  let pyrefly_results =
    Arg.(
      required & opt (some string) None & info ["pyrefly-results"] ~doc:"Path to Pyrefly results")
  in
  let query = Arg.(required & opt (some string) None & info ["query"] ~doc:"Query to run") in
  let term = Term.(const run_command $ pyrefly_results $ query) in
  let info = Cmd.info "pyrefly-query" ~doc in
  Cmd.v info term
