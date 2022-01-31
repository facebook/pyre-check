(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = {
  ordered: TaintTransform.t list;
  sanitize: SanitizeTransform.Set.t;
}
[@@deriving compare, eq]

let empty = { ordered = []; sanitize = SanitizeTransform.Set.empty }

let concat left right =
  {
    ordered = left.ordered @ right.ordered;
    sanitize = SanitizeTransform.Set.union left.sanitize right.sanitize;
  }
