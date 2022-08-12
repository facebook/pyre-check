(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module CodeNavigationConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    socket_path: PyrePath.t;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    critical_files: Server.CriticalFile.t list;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let critical_file_list_member =
            let to_critical_file json =
              Server.CriticalFile.of_yojson json |> Result.ok_or_failwith
            in
            list_member ~f:to_critical_file
          in

          let socket_path = json |> path_member "socket_path" in
          let watchman_root = json |> optional_path_member "watchman_root" in
          let critical_files = json |> critical_file_list_member "critical_files" ~default:[] in
          let additional_logging_sections =
            json |> string_list_member "additional_logging_sections" ~default:[]
          in
          Result.Ok
            { base; socket_path; watchman_root; critical_files; additional_logging_sections }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)
end

let run_server configuration_file =
  match
    CommandStartup.read_and_parse_json configuration_file ~f:CodeNavigationConfiguration.of_yojson
  with
  | Result.Error message ->
      Log.error "%s" message;
      exit 1
  | Result.Ok
      {
        CodeNavigationConfiguration.base =
          {
            CommandStartup.BaseConfiguration.log_path;
            global_root;
            local_root;
            debug;
            remote_logging;
            profiling_output;
            memory_profiling_output;
            _;
          };
        additional_logging_sections;
        _;
      } ->
      CommandStartup.setup_global_states
        ~global_root
        ~local_root
        ~debug
        ~additional_logging_sections
        ~remote_logging
        ~profiling_output
        ~memory_profiling_output
        ();

      (* Show start up notification. *)
      Server.StartupNotification.consume ~log_path ()
      |> Option.iter ~f:(fun message -> Log.warning "%s" message);

      (* Ignore SIGPIPE since >99% of the time they are non-fatal but the default Unix behavior is
         for it to terminate the server, which is not ideal. Besides, individual callsites can
         mostly detect the same class of issue by handling the EPIPE unix errno. *)
      Signal.Expert.(set Signal.pipe `Ignore);

      failwith "not implemented yet"


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Start a new Pyre server for code navigation purpose"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
