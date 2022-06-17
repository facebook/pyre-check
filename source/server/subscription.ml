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
    { Response.name; body = response }
    |> Response.to_yojson
    |> Yojson.Safe.to_string
    |> Lwt_io.write_line output_channel
    >>= fun () ->
    Lwt_io.flush output_channel
    >>= fun () ->
    Log.log ~section:`Server "Update sent to subscription `%s`" name;
    Lwt.return_unit)
  else (
    Log.warning
      "Trying to send updates to subscription `%s` whose output channel is already closed."
      name;
    Lwt.return_unit)


let batch_send ~response subscriptions =
  List.map subscriptions ~f:(fun subscription -> send ~response:(Lazy.force response) subscription)
  |> Lwt.join
