(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type Connections = sig
  val write_to_persistent_client
    :  connections:State.connections ->
    socket:Network.Socket.t ->
    response:Protocol.response ->
    unit

  (* Mutates the state to keep track of the socket. *)
  val add_persistent_client : connections:State.connections -> socket:Network.Socket.t -> unit

  val remove_persistent_client : connections:State.connections -> socket:Network.Socket.t -> unit

  val broadcast_response : connections:State.connections -> response:Protocol.response -> unit

  val add_json_socket : connections:State.connections -> socket:Network.Socket.t -> unit

  val remove_json_socket : connections:State.connections -> socket:Network.Socket.t -> unit

  val close_json_sockets : connections:State.connections -> unit

  val write_to_json_socket : socket:Network.Socket.t -> Yojson.Safe.t -> unit

  val write_lsp_response_to_json_socket : socket:Network.Socket.t -> string -> unit

  val add_adapter_socket : connections:State.connections -> socket:Network.Socket.t -> unit

  val remove_adapter_socket : connections:State.connections -> socket:Network.Socket.t -> unit

  val broadcast_to_adapter_sockets
    :  connections:State.connections ->
    response:Protocol.response ->
    unit
end

module Make (Socket : sig
  val write : Unix.File_descr.t -> Protocol.response -> unit

  val close : Unix.File_descr.t -> unit
end) : Connections = struct
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
              Map.remove persistent_clients socket)


  let broadcast_response ~connections:{ State.lock; connections; _ } ~response =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
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
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        let persistent_clients = write_to_persistent_client persistent_clients ~socket ~response in
        connections := { cached_connections with persistent_clients })


  let add_persistent_client ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        connections :=
          {
            cached_connections with
            persistent_clients = Map.set persistent_clients ~key:socket ~data:0;
          })


  let remove_persistent_client ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.persistent_clients; _ } as cached_connections) = !connections in
        Socket.close socket;
        connections :=
          { cached_connections with persistent_clients = Map.remove persistent_clients socket })


  let add_json_socket ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let { State.json_sockets; _ } = !connections in
        connections := { !connections with json_sockets = socket :: json_sockets })


  let remove_json_socket ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.json_sockets; sockets_to_close; _ } as cached_connections) = !connections in
        let json_sockets, to_close =
          List.partition_tf
            ~f:(fun file_notifier_socket ->
              if Unix.File_descr.equal socket file_notifier_socket then (
                Log.log ~section:`Server "Adding json socket to be removed.";
                false)
              else
                true)
            json_sockets
        in
        let sockets_to_close = List.append sockets_to_close to_close in
        connections := { cached_connections with json_sockets; sockets_to_close })


  let close_json_sockets ~connections:{ State.lock; connections; _ } =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.sockets_to_close; _ } as cached_connections) = !connections in
        List.iter sockets_to_close ~f:(fun socket -> Socket.close socket);
        connections := { cached_connections with sockets_to_close = [] })


  let write_to_json_socket ~socket message =
    let out_channel = Unix.out_channel_of_descr socket in
    LanguageServer.Protocol.write_message out_channel message;
    Out_channel.flush out_channel


  let write_lsp_response_to_json_socket ~socket message =
    let out_channel = Unix.out_channel_of_descr socket in
    LanguageServer.Protocol.write_adapter_message out_channel message;
    Out_channel.flush out_channel


  let add_adapter_socket ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let { State.adapter_sockets; _ } = !connections in
        connections := { !connections with adapter_sockets = socket :: adapter_sockets })


  let remove_adapter_socket ~connections:{ State.lock; connections; _ } ~socket =
    Error_checking_mutex.critical_section lock ~f:(fun () ->
        let ({ State.adapter_sockets; _ } as cached_connections) = !connections in
        Socket.close socket;
        let adapter_sockets =
          List.filter adapter_sockets ~f:(fun adapter_socket ->
              not (Unix.File_descr.equal socket adapter_socket))
        in
        connections := { cached_connections with adapter_sockets })


  let broadcast_to_adapter_sockets ~connections:{ State.lock; connections; _ } ~response =
    match response with
    | Protocol.LanguageServerProtocolResponse response ->
        Error_checking_mutex.critical_section lock ~f:(fun () ->
            let ({ State.adapter_sockets; _ } as cached_connections) = !connections in
            let adapter_sockets =
              List.filter adapter_sockets ~f:(fun socket ->
                  try
                    write_lsp_response_to_json_socket ~socket response;
                    true
                  with
                  | Unix.Unix_error (name, kind, parameters) ->
                      Log.log_unix_error (name, kind, parameters);
                      false
                  | _ ->
                      Log.error "Socket error";
                      false)
            in
            connections := { cached_connections with adapter_sockets })
    | _ -> ()
end

module Unix = Make (struct
  let write = Network.Socket.write

  let close descriptor = Unix.close descriptor
end)
