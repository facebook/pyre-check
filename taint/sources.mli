(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | Test
  | Thrift
  | UserControlled
[@@deriving compare, sexp, show, hash]

val create: string -> t
