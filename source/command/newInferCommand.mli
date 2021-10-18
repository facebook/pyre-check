(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module InferConfiguration : sig
  type file_list = string list option [@@deriving yojson, sexp, compare, hash]

  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    paths_to_modify: file_list;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t
