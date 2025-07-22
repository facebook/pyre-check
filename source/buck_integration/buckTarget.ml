(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module provides an interface describing a Buck target *)

open Base

type t = string [@@deriving sexp, compare, hash]

let of_string target = target

let show target = target

let pp formatter target = Stdlib.Format.fprintf formatter "%s" target
