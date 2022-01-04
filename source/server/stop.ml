(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let stop_waiting_server () =
  (* Send the process itself a SIGINT. *)
  let () = Signal.send_exn Signal.int (`Pid (Unix.getpid ())) in
  (* Block forever and wait for the signal to be caught. This way, client who requested the stop can
     actually tell when the server is down by monitoring when its connection with the server gets
     dropped. *)
  let wait_forever, _ = Lwt.wait () in
  wait_forever


let log_stopped_server ~reason ~start_time () =
  Statistics.event
    ~flush:true
    ~name:"stop server"
    ~normals:["reason", reason; "server_version", Version.version ()]
    ~integers:["up_time", Timer.stop_in_ms start_time]
    ()


let log_and_stop_waiting_server ~reason ~properties:{ ServerProperties.start_time; _ } () =
  log_stopped_server ~reason ~start_time ();
  stop_waiting_server ()
