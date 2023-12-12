(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val wrap : exn -> t

val unwrap : t -> exn

val reraise : t -> 'a

val raise_with_backtrace : exn -> t -> 'a

val to_string : t -> string

val exn_to_string : exn -> string

val get_exn_string : t -> string

val get_backtrace_string : t -> string

val get_current_callstack_string : int -> string

val record_backtrace : bool -> unit

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
