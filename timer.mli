(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t

val start: unit -> t
val stop: t -> float
val stop_in_ms: t -> int
