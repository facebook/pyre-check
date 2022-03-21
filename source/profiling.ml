(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Worker = Hack_parallel.Std.Worker

module GlobalState = struct
  type t = {
    mutable profiling_output: string option;
    mutable memory_profiling_output: string option;
  }

  let global_state = { profiling_output = None; memory_profiling_output = None }

  let initialize ?profiling_output ?memory_profiling_output () =
    Option.iter profiling_output ~f:(fun output ->
        PyrePath.remove_if_exists (PyrePath.create_absolute output);
        global_state.profiling_output <- Some output);
    Option.iter memory_profiling_output ~f:(fun output ->
        PyrePath.remove_if_exists (PyrePath.create_absolute output);
        global_state.memory_profiling_output <- Some output);
    ()


  let get () = global_state

  let restore old_state =
    global_state.profiling_output <- old_state.profiling_output;
    global_state.memory_profiling_output <- old_state.memory_profiling_output
end

module Event = struct
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

  let now_in_microseconds () =
    let now_in_nanoseconds = Mtime_clock.now () |> Mtime.to_uint64_ns |> Int.of_int64_trunc in
    now_in_nanoseconds / 1000


  let create ?(timestamp = now_in_microseconds ()) ?(tags = []) ~event_type name =
    let name = String.filter ~f:Char.is_print name in
    let pid = Unix.getpid () |> Pid.to_int in
    let worker_id = Worker.current_worker_id () in
    { name; worker_id; pid; event_type; timestamp; tags }
end

let log_to_path path ~event_creator =
  let path = PyrePath.create_absolute path in
  let line = event_creator () |> Event.to_yojson |> Yojson.Safe.to_string in
  File.append ~lines:[line] path


(* Taking a constructor instead of an event here so that events can be created lazily *)
let log_performance_event event_creator =
  Option.iter GlobalState.global_state.profiling_output ~f:(log_to_path ~event_creator)


let log_memory_event event_creator =
  Option.iter GlobalState.global_state.memory_profiling_output ~f:(log_to_path ~event_creator)


type 'a result_with_tags = {
  result: 'a;
  tags: unit -> (string * string) list;
}

let track_duration_event_with_dynamic_tags ~f name =
  let timer = Timer.start () in
  let { result; tags } = f () in
  let duration = Timer.stop_in_us timer in
  let create_event () =
    let tags = tags () in
    Event.create name ~tags ~event_type:(Duration duration)
  in
  log_performance_event create_event;
  result


let track_duration_event ?(tags = []) ~f name =
  let f () = { result = f (); tags = (fun () -> tags) } in
  track_duration_event_with_dynamic_tags ~f name


let track_shared_memory_usage ?name () =
  let create_event () =
    let used_heap_size = SharedMemory.heap_size () in
    let wasted_heap_size = SharedMemory.wasted_heap_size () in
    let { SharedMemory.nonempty_slots = nonempty_hash_slots; used_slots = used_hash_slots; _ } =
      SharedMemory.hash_stats ()
    in
    let { SharedMemory.used_slots = used_dependency_slots; _ } = SharedMemory.dep_stats () in
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
  log_memory_event create_event


let with_before_and_after_shared_memory_tracking f ~name =
  track_shared_memory_usage () ~name:(Format.sprintf "Before %s" name);
  let result = f () in
  track_shared_memory_usage () ~name:(Format.sprintf "After %s" name);
  result


let track_duration_and_shared_memory ?tags ~f name =
  with_before_and_after_shared_memory_tracking (fun () -> track_duration_event name ?tags ~f) ~name


let track_duration_and_shared_memory_with_dynamic_tags ~f name =
  with_before_and_after_shared_memory_tracking
    (fun () -> track_duration_event_with_dynamic_tags name ~f)
    ~name
