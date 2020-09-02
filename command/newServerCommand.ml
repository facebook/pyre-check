(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let read_json file_path =
  try Result.Ok (Yojson.Safe.from_file file_path) with
  | Yojson.Json_error message
  | Sys_error message ->
      Result.Error message
  | other_exception -> Result.Error (Exn.to_string other_exception)


let run_server configuration_file =
  match read_json configuration_file with
  | Result.Error message ->
      Log.error "Cannot read JSON file.";
      Log.error "%s" message
  | Result.Ok configuration_json -> (
      match Newserver.ServerConfiguration.of_yojson configuration_json with
      | Result.Error message ->
          Log.error "Malformed server specification JSON.";
          Log.error "%s" message
      | Result.Ok ({ Newserver.ServerConfiguration.log_path; _ } as server_configuration) ->
          Newserver.StartupNotification.consume ~log_path ()
          |> Option.iter ~f:(fun message -> Log.warning "%s" message);
          Lwt_main.run (Newserver.Start.start_server_and_wait server_configuration) )


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Starts a new Pyre server."
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
