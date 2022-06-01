(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Target.t list Target.Map.t

val to_alist : ?key_order:[ `Decreasing | `Increasing ] -> t -> (Target.t * Target.t list) list

val dump : path:PyrePath.t -> t -> unit

val pp : Format.formatter -> t -> unit
