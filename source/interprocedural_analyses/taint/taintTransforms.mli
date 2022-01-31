(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  ordered: TaintTransform.t list;
  sanitize: SanitizeTransform.Set.t;
}
[@@deriving compare, eq]

val empty : t

val concat : t -> t -> t
