(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

module Reason = struct
  type t =
    | ExplicitRequest
    | CriticalFileUpdate of PyrePath.t
    | UncaughtException of exn

  let name_of = function
    | ExplicitRequest -> "explicit request"
    | CriticalFileUpdate _ -> "critical file update"
    | UncaughtException _ -> "uncaught exception"


  let origin_of_exception exn =
    let kind, _ = ServerError.kind_and_message_from_exception exn in
    match kind with
    | ServerError.Kind.Watchman -> "watchman"
    | ServerError.Kind.BuckInternal
    | ServerError.Kind.BuckUser ->
        "buck"
    | ServerError.Kind.Pyre
    | ServerError.Kind.Unknown ->
        "server"


  let message_of = function
    | ExplicitRequest -> "Pyre server stopped because one client explicitly sent a `stop` request"
    | CriticalFileUpdate path ->
        Format.asprintf
          "Pyre server needs to restart as it is notified on potential changes in `%a`"
          PyrePath.pp
          path
    | UncaughtException exn ->
        Format.sprintf
          "Pyre server stopped due to uncaught exception (origin: %s)"
          (origin_of_exception exn)
end

let last_server_stop_reason = ref None

let get_last_server_stop_reason () = !last_server_stop_reason

let stop_waiting_server reason =
  last_server_stop_reason := Some reason;
  (* Send the process itself a SIGINT. *)
  let () = Signal_unix.send_exn Signal.int (`Pid (Core_unix.getpid ())) in
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
