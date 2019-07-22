(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type t = Time_stamp_counter.t

let calibrator = Time_stamp_counter.Calibrator.create ()

let start () = Time_stamp_counter.now ()

let span start_time =
  let stop_time = Time_stamp_counter.now () in
  let timestamp_span = Time_stamp_counter.diff stop_time start_time in
  Time_stamp_counter.Span.to_time_span ~calibrator timestamp_span


let stop start_time = start_time |> span

let stop_in_ms start_time = stop start_time |> Time.Span.to_ms |> Int.of_float
