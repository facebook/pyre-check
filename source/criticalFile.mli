(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | BaseName of string
  | Extension of string
  | FullPath of PyrePath.t
[@@deriving sexp, compare, hash, yojson]

val matches : path:PyrePath.t -> t -> bool

val matches_any : path:PyrePath.t -> t list -> bool

val find : within:PyrePath.t list -> t list -> PyrePath.t option
