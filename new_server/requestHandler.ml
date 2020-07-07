(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

let process_request ~state:({ ServerState.socket_path } as state) request =
  match request with
  | Request.GetInfo ->
      let response =
        Response.Info
          {
            version = Version.version ();
            pid = Unix.getpid () |> Pid.to_int;
            socket = Pyre.Path.absolute socket_path;
          }
      in
      Lwt.return (state, response)
  | Request.Stop -> Stop.stop_waiting_server ()
