(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type t [@@deriving eq, sexp, show, compare]

val empty_stub : t -> bool

val from_empty_stub : reference:Reference.t -> module_definition:(Reference.t -> t option) -> bool

val aliased_export : t -> Reference.t -> Reference.t option

module Cache : sig
  val clear : unit -> unit
end

val create : Source.t -> t

val create_implicit : ?empty_stub:bool -> unit -> t

(* Exposed for testing only *)
val create_for_testing : local_mode:Source.mode -> stub:bool -> t
