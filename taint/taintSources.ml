(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type t =
  | UserControlled
[@@deriving compare, sexp, show, hash]


let to_string = function
  | UserControlled -> "user controlled"
