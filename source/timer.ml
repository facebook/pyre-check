(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = Mtime_clock.counter

let start () = Mtime_clock.counter ()

let stop counter = Mtime_clock.count counter

let stop_in_sec counter = stop counter |> Mtime.Span.to_s

let stop_in_ms counter = stop counter |> Mtime.Span.to_ms |> Int.of_float

let stop_in_us counter = stop counter |> Mtime.Span.to_us |> Int.of_float
