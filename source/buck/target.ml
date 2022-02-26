(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = string [@@deriving sexp, compare, hash]

let of_string target = target

let show target = target

let pp formatter target = Format.fprintf formatter "%s" target
