(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base

type t = Mtime_clock.counter

let start () = Mtime_clock.counter ()

let stop counter = Mtime_clock.count counter

let stop_in_sec counter =
  Int64.(Mtime.Span.to_uint64_ns (stop counter) / 1_000_000_000L)
  |> Int64.to_float

let stop_in_ms counter =
  Int64.(Mtime.Span.to_uint64_ns (stop counter) / 1_000_000L)
  |> Int.of_int64_exn

let stop_in_us counter =
  Int64.(Mtime.Span.to_uint64_ns (stop counter) / 1_000L) |> Int.of_int64_exn
