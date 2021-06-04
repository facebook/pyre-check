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
      | Result.Ok
          ( {
              Newserver.ServerConfiguration.log_path;
              global_root;
              local_root;
              debug;
              additional_logging_sections;
              remote_logging;
              profiling_output;
              memory_profiling_output;
              _;
            } as server_configuration ) ->
          (* Set up global states. *)
          Log.GlobalState.initialize ~debug ~sections:additional_logging_sections;
          let logger, log_identifier =
            match remote_logging with
            | None -> None, ""
            | Some { Configuration.RemoteLogging.logger; identifier } -> Some logger, identifier
          in
          let relative_local_root =
            match local_root with
            | None -> ""
            | Some local_root ->
                PyrePath.get_relative_to_root ~root:global_root ~path:local_root
                |> Option.value ~default:""
          in
          Statistics.GlobalState.initialize
            ~log_identifier
            ?logger
            ~project_root:(PyrePath.absolute global_root)
            ~project_name:relative_local_root
            ();
          Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();

          (* Show start up notification. *)
          Newserver.StartupNotification.consume ~log_path ()
          |> Option.iter ~f:(fun message -> Log.warning "%s" message);

          let exit_status =
            Lwt_main.run
              (Newserver.Start.start_server_and_wait
                 ~event_channel:Lwt_io.stdout
                 server_configuration)
          in
          exit (Newserver.Start.ExitStatus.exit_code exit_status) )


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Starts a new Pyre server."
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
