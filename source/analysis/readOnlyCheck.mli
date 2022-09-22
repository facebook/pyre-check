(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ReadOnlyness : sig
  type t =
    | Mutable
    | ReadOnly
  [@@deriving compare, sexp]

  include Abstract.SimpleDomain.ELEMENT with type t := t
end
