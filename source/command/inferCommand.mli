(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module InferConfiguration : sig
  type path_list = PyrePath.t list [@@deriving sexp, compare, hash]

  type t = {
    base: CommandStartup.BaseConfiguration.t;
    paths_to_modify: path_list option;
  }
  [@@deriving sexp, compare, hash]

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

val command : Command.t
