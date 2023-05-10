(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
module ServerResponse = Response

module Request = struct
  type t =
    | SubscribeToTypeErrors of string
    | SubscribeToStateChanges of string
  [@@deriving equal, yojson { strict = false }]
end

module Response = struct
  type t = {
    name: string;
    body: Response.t;
  }
  [@@deriving equal, to_yojson { strict = false }]

  let send ~output_channel response =
    let open Lwt.Infix in
    let raw_response = Yojson.Safe.to_string (to_yojson response) in
    Lwt_io.write_line output_channel raw_response >>= fun () -> Lwt_io.flush output_channel


  let send_ignoring_errors ~output_channel response =
    let on_io_exception exn =
      Log.log
        ~section:`Server
        "Exception occurred while sending subscription responses: %s"
        (Exn.to_string exn);
      Lwt.return_unit
    in
    Lwt.catch (fun () -> send ~output_channel response) on_io_exception
end

module Kind = struct
  type t =
    | TypeErrors
    | StateChanges
end

type t = {
  name: string;
  kind: Kind.t;
  output_channel: Lwt_io.output_channel;
}

let wants_type_errors { kind; _ } =
  match kind with
  | TypeErrors -> true
  | StateChanges -> false


let create ~subscription_request ~output_channel () =
  match subscription_request with
  | Request.SubscribeToTypeErrors name -> { name; kind = Kind.TypeErrors; output_channel }
  | Request.SubscribeToStateChanges name -> { name; kind = Kind.StateChanges; output_channel }


let name_of { name; _ } = name

let send ~response { name; output_channel; _ } =
  if not (Lwt_io.is_closed output_channel) then (
    let open Lwt.Infix in
    Response.send_ignoring_errors ~output_channel { Response.name; body = response }
    >>= fun () ->
    Log.log ~section:`Server "Update sent to subscription `%s`" name;
    Lwt.return_unit)
  else (
    Log.log
      ~section:`Server
      "Trying to send updates to subscription `%s` whose output channel is already closed."
      name;
    Lwt.return_unit)


let batch_send ~response subscriptions =
  List.map subscriptions ~f:(fun subscription -> send ~response:(Lazy.force response) subscription)
  |> Lwt.join
