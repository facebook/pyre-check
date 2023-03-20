(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* File containing useful general algorithm *)

open Core

let fold_balanced list ~f ~init =
  Option.value (List.reduce_balanced ~f (init :: list)) ~default:init
