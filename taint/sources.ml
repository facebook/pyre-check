(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type t =
  | Test
  | Thrift
  | UserControlled
[@@deriving compare, sexp, show, hash]


let show = function
  | Test -> "Test"
  | Thrift -> "Thrift"
  | UserControlled -> "UserControlled"


let create = function
  | "Test" -> Test
  | "Thrift" -> Thrift
  | "UserControlled" -> UserControlled
  | name -> failwith (Format.sprintf "Unsupported taint source %s" name)
