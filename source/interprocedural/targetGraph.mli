(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Target.t list Target.Map.Tree.t

val to_alist : sorted:bool -> t -> (Target.t * Target.t list) list

val dump : path:PyrePath.t -> t -> unit

val pp : Format.formatter -> t -> unit

val to_json : skip_empty_callees:bool -> sorted:bool -> t -> Yojson.Safe.t
