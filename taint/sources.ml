(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type t =
  | TestSource
  | UserControlled
[@@deriving compare, sexp, show, hash]


let create = function
  | "TestSource" -> TestSource
  | "UserControlled" -> UserControlled
  | name -> failwith (Format.sprintf "Unsupported taint source %s" name)
