(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val start : start_message:string -> end_message:string -> unit -> t

val finish : ?integers:(string * int) list -> t -> unit
