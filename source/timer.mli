(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t

val calibrator : Time_stamp_counter.Calibrator.t

val start : unit -> t

val stop : t -> Time.Span.t

val stop_in_ms : t -> int

val stop_in_us : t -> int
