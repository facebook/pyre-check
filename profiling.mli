(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Event : sig
  type event_type = Duration of int [@@deriving yojson]

  type t = {
    name: string;
    pid: int;
    event_type: event_type;
    timestamp: int;
    tags: (string * string) list;
  }
  [@@deriving yojson]

  val create
    :  ?timestamp:int ->
    ?tags:(string * string) list ->
    event_type:event_type ->
    string ->
    t
end

val log_event : Event.t -> unit

val track_duration_event : ?tags:(string * string) list -> f:(unit -> 'a) -> string -> 'a
