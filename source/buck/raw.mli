(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains the low-level interfaces for invoking `buck` as an external tool. *)

exception
  BuckError of {
    arguments: string list;
    description: string;
  }
(** Raised when external invocation of `buck` returns an error. *)

type t

val create : unit -> t

(* This API is useful for testing. It allows its client to competely mock out buck behavior by
   specifying what to return when other APIs are invoked. *)
val create_for_testing
  :  query:(string list -> string Lwt.t) ->
  build:(string list -> string Lwt.t) ->
  unit ->
  t

val query : t -> string list -> string Lwt.t
(** Shell out to `buck query` with the given cli arguments. Returns the content of stdout. If the
    return code is not 0, raise [BuckError]. *)

val build : t -> string list -> string Lwt.t
(** Shell out to `buck build` with the given cli arguments. Returns the content of stdout. If the
    return code is not 0, raise [BuckError]. *)
