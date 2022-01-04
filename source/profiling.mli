(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module GlobalState : sig
  type t

  val initialize : ?profiling_output:string -> ?memory_profiling_output:string -> unit -> unit

  val get : unit -> t

  val restore : t -> unit
end

module Event : sig
  type event_type =
    | Duration of int
    | Counter of string option
  [@@deriving yojson]

  type t = {
    name: string;
    worker_id: int;
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

val log_performance_event : (unit -> Event.t) -> unit

val track_duration_event : ?tags:(string * string) list -> f:(unit -> 'a) -> string -> 'a

type 'a result_with_tags = {
  result: 'a;
  tags: unit -> (string * string) list;
}

val track_duration_event_with_dynamic_tags : f:(unit -> 'a result_with_tags) -> string -> 'a

val track_shared_memory_usage : ?name:string -> unit -> unit

val track_duration_and_shared_memory
  :  ?tags:(string * string) list ->
  f:(unit -> 'a) ->
  string ->
  'a

val track_duration_and_shared_memory_with_dynamic_tags
  :  f:(unit -> 'a result_with_tags) ->
  string ->
  'a
(** Convenient functions to track both time and shared memory usage *)
