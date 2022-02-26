(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val start : unit -> t

val stop_in_sec : t -> float

val stop_in_ms : t -> int

val stop_in_us : t -> int
