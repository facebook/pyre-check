(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module Make (Socket : sig
  val write : Unix.File_descr.t -> Protocol.response -> unit

  val close : Unix.File_descr.t -> unit
end) =
struct
  let failure_threshold = 5

  (* This function is not exposed - it's used in the body of broadcast_response and
     write_to_persistent_client, and assumes that the lock is held when called. *)
  let write_to_persistent_client persistent_clients ~socket ~response =
    if not (Map.mem persistent_clients socket) then
      persistent_clients
    else
      try
        Socket.write socket response;
        persistent_clients
      with
      | Unix.Unix_error (Unix.EPIPE, _, _) -> (
          Log.warning "Got an EPIPE while writing to a persistent client";
          match Map.find_exn persistent_clients socket with
          | failure when failure + 1 < failure_threshold ->
              Map.set persistent_clients ~key:socket ~data:(failure + 1)
          | _ ->
              Socket.close socket;
              Map.remove persistent_clients socket )


  let broadcast_response ~connections:{ State.lock; connections; _ } ~response =
    Mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        let persistent_clients =
          Map.fold
            persistent_clients
            ~init:persistent_clients
            ~f:(fun ~key:socket ~data:_ persistent_clients ->
              write_to_persistent_client persistent_clients ~socket ~response)
        in
        connections := { cached_connections with persistent_clients })


  let write_to_persistent_client ~connections:{ State.lock; connections; _ } ~socket ~response =
    Mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        let persistent_clients = write_to_persistent_client persistent_clients ~socket ~response in
        connections := { cached_connections with persistent_clients })


  let add_persistent_client ~connections:{ State.lock; connections; _ } ~socket =
    Mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        connections :=
          { cached_connections with
            persistent_clients = Map.set persistent_clients ~key:socket ~data:0
          })


  let remove_persistent_client ~connections:{ State.lock; connections; _ } ~socket =
    Mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        Socket.close socket;
        connections :=
          { cached_connections with persistent_clients = Map.remove persistent_clients socket })
end
