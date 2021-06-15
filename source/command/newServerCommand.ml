(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let run_server configuration_file =
  match
    NewCommandStartup.read_and_parse_json
      configuration_file
      ~f:Newserver.ServerConfiguration.of_yojson
  with
  | Result.Error message ->
      Log.error "%s" message;
      exit 1
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
      NewCommandStartup.setup_global_states
        ~global_root
        ~local_root
        ~debug
        ~additional_logging_sections
        ~remote_logging
        ~profiling_output
        ~memory_profiling_output
        ();

      (* Show start up notification. *)
      Newserver.StartupNotification.consume ~log_path ()
      |> Option.iter ~f:(fun message -> Log.warning "%s" message);

      let exit_status =
        Lwt_main.run
          (Newserver.Start.start_server_and_wait ~event_channel:Lwt_io.stdout server_configuration)
      in
      exit (Newserver.Start.ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Starts a new Pyre server."
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
