(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module Identifier = struct
  type t = int [@@deriving sexp, equal]
end

type t = {
  next_identifier: int ref;
  registered: Lwt_io.output_channel Hashtbl.M(Int).t;
}

let create () = { next_identifier = ref 0; registered = Hashtbl.create (module Int) }

let count { registered; _ } = Hashtbl.length registered

let register ~output_channel { next_identifier; registered } =
  let key = !next_identifier in
  incr next_identifier;
  (* NOTE(grievejia): This [add_exn] is safe unless the number of subscriptions exceeds
     [Int.max_value], which seems unlikely. *)
  Hashtbl.add_exn registered ~key ~data:output_channel;
  key


let unregister ~identifier { registered; _ } = Hashtbl.remove registered identifier

let broadcast_raw ~message { registered; _ } =
  Hashtbl.data registered
  |> List.map ~f:(fun output_channel ->
         LwtInputOutput.write_line_ignoring_errors ~output_channel (Lazy.force message))
  |> Lwt.join


let broadcast ~response subscriptions =
  let message =
    lazy (Lazy.force response |> Subscription.Response.to_yojson |> Yojson.Safe.to_string)
  in
  broadcast_raw subscriptions ~message
