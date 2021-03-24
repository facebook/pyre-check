(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains high-level interfaces for invoking `buck` as an external tool. *)

type t

exception JsonError of string
(** Raised when `buck` returns malformed JSONs *)

val create : ?mode:string -> ?isolation_prefix:string -> Raw.t -> t

val normalize_targets : t -> string list -> Target.t list Lwt.t
(** Given a list of buck target specifications (which may contain `...` or filter expression), query
    `buck` and return the set of individual targets which will be built. May raise
    [Buck.Raw.BuckError] when `buck` invocation fails, or [Buck.Builder.JsonError] when `buck`
    itself succeeds but its output cannot be parsed. *)

(* Raise [JsonError] on parsing error. Exposed for testing. *)
val parse_buck_query_output : string -> string list
