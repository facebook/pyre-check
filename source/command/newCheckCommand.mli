(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ExitStatus : sig
  type t =
    | Ok
    | PyreError
    | BuckInternalError
    | BuckUserError
  [@@deriving sexp, compare, hash]

  val exit_code : t -> int
end

module CheckConfiguration : sig
  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    strict: bool;
    show_error_traces: bool;
    additional_logging_sections: string list;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t

val on_exception : exn -> ExitStatus.t Lwt.t
