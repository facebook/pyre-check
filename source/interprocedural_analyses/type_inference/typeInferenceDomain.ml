(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type value =
  | Top
  | Bottom
[@@deriving show]

module Domain = struct
  type t = value [@@deriving show]

  let bottom = Bottom

  let join left right =
    match left, right with
    | Top, _
    | _, Top ->
        Top
    | Bottom, Bottom -> Bottom


  let less_or_equal ~left ~right =
    match left, right with
    | _, Top -> true
    | Bottom, _ -> true
    | Top, Bottom -> false
end

include Domain
