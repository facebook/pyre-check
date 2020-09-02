(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
