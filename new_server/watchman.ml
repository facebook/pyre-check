(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

exception ConnectionError of string

module Raw = struct
  module Response = struct
    type t =
      | Ok of Yojson.Safe.t
      | EndOfStream
      | Error of string
  end

  module Connection = struct
    type t = {
      send: Yojson.Safe.t -> unit Lwt.t;
      receive: unit -> Response.t Lwt.t;
      shutdown: unit -> unit Lwt.t;
    }

    let send { send; _ } = send

    let receive { receive; _ } = receive

    let shutdown { shutdown; _ } = shutdown
  end

  type t = { open_connection: unit -> Connection.t Lwt.t }

  let open_connection { open_connection } = open_connection ()

  let shutdown_connection connection = Connection.shutdown connection ()

  let with_connection ~f { open_connection } =
    let open Lwt.Infix in
    open_connection ()
    >>= fun connection -> Lwt.finalize (fun () -> f connection) (Connection.shutdown connection)


  let create_for_testing ~send ~receive () =
    let receive () =
      let open Lwt.Infix in
      receive ()
      >>= function
      | Some json -> Lwt.return (Response.Ok json)
      | None -> Lwt.return Response.EndOfStream
    in
    let shutdown () = Lwt.return_unit in
    let mock_connection = { Connection.send; receive; shutdown } in
    { open_connection = (fun () -> Lwt.return mock_connection) }


  let get_watchman_socket_name () =
    let open Lwt.Infix in
    LwtSubprocess.run "watchman" ~arguments:["--no-pretty"; "get-sockname"]
    >>= fun { LwtSubprocess.Completed.status; stdout; stderr } ->
    match status with
    | Caml.Unix.WEXITED 0 ->
        let socket_name =
          try
            Yojson.Safe.from_string stdout
            |> Yojson.Safe.Util.member "sockname"
            |> Yojson.Safe.Util.to_string
          with
          | Yojson.Json_error message ->
              let message =
                Format.sprintf "Cannot parse JSON result from watchman getsockname: %s" message
              in
              raise (ConnectionError message)
        in
        Lwt.return socket_name
    | WEXITED 127 ->
        let message =
          Format.sprintf
            "Cannot find watchman exectuable under PATH: %s"
            (Option.value (Sys_utils.getenv_path ()) ~default:"(not set)")
        in
        raise (ConnectionError message)
    | WEXITED code ->
        let message = Format.sprintf "Watchman exited code %d, stderr = %S" code stderr in
        raise (ConnectionError message)
    | WSIGNALED signal ->
        let message =
          Format.sprintf "watchman signaled with %s signal" (PrintSignal.string_of_signal signal)
        in
        raise (ConnectionError message)
    | WSTOPPED signal ->
        let message =
          Format.sprintf "watchman stopped with %s signal" (PrintSignal.string_of_signal signal)
        in
        raise (ConnectionError message)


  let create_exn () =
    let open Lwt.Infix in
    Log.info "Initializing file watching service...";
    get_watchman_socket_name ()
    >>= fun socket_name ->
    let open_connection () =
      Log.info "Connecting to watchman...";
      Lwt_io.open_connection (Lwt_unix.ADDR_UNIX socket_name)
      >>= fun (input_channel, output_channel) ->
      Log.info "Established watchman connection.";
      let send json = Yojson.Safe.to_string json |> Lwt_io.write_line output_channel in
      let receive () =
        Lwt_io.read_line_opt input_channel
        >>= function
        | None -> Lwt.return Response.EndOfStream
        | Some line -> (
            try
              let json = Yojson.Safe.from_string line in
              Lwt.return (Response.Ok json)
            with
            | Yojson.Json_error message ->
                let message =
                  Format.sprintf "Cannot parse JSON from watchman response: %s" message
                in
                Lwt.return (Response.Error message) )
      in
      let shutdown () =
        Log.info "Shutting down watchman connection...";
        Lwt_io.close input_channel >>= fun () -> Lwt_io.close output_channel
      in
      Lwt.return { Connection.send; receive; shutdown }
    in
    Lwt.return { open_connection }


  let create () =
    let open Lwt.Infix in
    Lwt.catch
      (fun () -> create_exn () >>= fun raw -> Lwt.return (Result.Ok raw))
      (fun exn ->
        let message =
          Format.sprintf "Cannot initialize watchman due to exception: %s" (Exn.to_string exn)
        in
        Lwt.return (Result.Error message))
end
