(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = Named of string [@@deriving compare, eq, hash, sexp]

let pp formatter = function
  | Named transform -> Format.fprintf formatter "%s" transform


let show = Format.asprintf "%a" pp
