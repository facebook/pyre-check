(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Event = struct
  type event_type =
    | Duration of int
    | Counter of string option
  [@@deriving yojson]

  type t = {
    name: string;
    pid: int;
    event_type: event_type;
    timestamp: int;
    tags: (string * string) list;
  }
  [@@deriving yojson]

  let now_in_milliseconds () =
    Time_stamp_counter.now ()
    |> Time_stamp_counter.to_time ~calibrator:Timer.calibrator
    |> Time.to_span_since_epoch
    |> Time.Span.to_ms
    |> Int.of_float


  let create ?(timestamp = now_in_milliseconds ()) ?(tags = []) ~event_type name =
    let name = String.filter ~f:Char.is_print name in
    let pid = Unix.getpid () |> Pid.to_int in
    { name; pid; event_type; timestamp; tags }
end

(* Taking a constructor instead of an event here so that events can be created lazily *)
let log_event event_creator =
  let log_to_path path =
    let path = Path.create_absolute ~follow_symbolic_links:false path in
    let line = event_creator () |> Event.to_yojson |> Yojson.Safe.to_string in
    File.append ~lines:[line] path
  in
  Configuration.Analysis.get_global ()
  >>= (fun { Configuration.Analysis.profiling_output; _ } -> profiling_output)
  |> Option.iter ~f:log_to_path


let track_duration_event ?(tags = []) ~f name =
  let timer = Timer.start () in
  let result = f () in
  let duration = Timer.stop_in_ms timer in
  let create_event () = Event.create name ~tags ~event_type:(Duration duration) in
  log_event create_event;
  result


let track_shared_memory_usage ?name () =
  let create_event () =
    let used_heap_size = SharedMem.heap_size () in
    let wasted_heap_size = SharedMem.wasted_heap_size () in
    let { SharedMem.nonempty_slots = nonempty_hash_slots; used_slots = used_hash_slots; _ } =
      SharedMem.hash_stats ()
    in
    let { SharedMem.used_slots = used_dependency_slots; _ } = SharedMem.dep_stats () in
    let create_tag name counter = name, string_of_int counter in
    Event.create
      "Shared Memory Usage"
      ~event_type:(Event.Counter name)
      ~tags:
        [
          create_tag "used_heap_size" used_heap_size;
          create_tag "wasted_heap_size" wasted_heap_size;
          create_tag "nonempty_hash_slots" nonempty_hash_slots;
          create_tag "used_hash_slots" used_hash_slots;
          create_tag "used_dependency_slots" used_dependency_slots;
        ]
  in
  log_event create_event


let track_duration_and_shared_memory ~f name =
  track_shared_memory_usage () ~name:(Format.sprintf "Before [%s]" name);
  let result = track_duration_event name ~f in
  track_shared_memory_usage () ~name:(Format.sprintf "After [%s]" name);
  result
