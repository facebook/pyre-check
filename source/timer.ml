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

let stop_in_us counter =
  let ns = Mtime.Span.to_uint64_ns (stop counter) in
  Int64.(to_int_trunc (ns / 1000L))


let stop_in_ms counter = stop_in_us counter / 1000

let stop_in_sec counter = Float.of_int (stop_in_ms counter) /. 1000.
