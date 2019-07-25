(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Event = struct
  type event_type = Duration of int [@@deriving yojson]

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

let log_event event =
  let log_to_path path =
    let path = Path.create_absolute ~follow_symbolic_links:false path in
    let line = event |> Event.to_yojson |> Yojson.Safe.to_string in
    File.append ~lines:[line] path
  in
  Configuration.Analysis.get_global ()
  >>= (fun { Configuration.Analysis.profiling_output; _ } -> profiling_output)
  |> Option.iter ~f:log_to_path


let track_duration_event ?(tags = []) ~f name =
  let timer = Timer.start () in
  let result = f () in
  let duration = Timer.stop_in_ms timer in
  log_event (Event.create name ~tags ~event_type:(Duration duration));
  result
