(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module ServerResponse = Response

module Request = struct
  type t = SubscribeToTypeErrors of string [@@deriving sexp, compare, yojson { strict = false }]
end

module Response = struct
  type t = {
    name: string;
    body: Response.t;
  }
  [@@deriving sexp, compare, to_yojson { strict = false }]

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

type t = {
  name: string;
  output_channel: Lwt_io.output_channel;
}

let create ~name ~output_channel () = { name; output_channel }

let name_of { name; _ } = name

let send ~response { name; output_channel } =
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
