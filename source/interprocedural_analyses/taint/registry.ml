(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Registry: represents a mapping from targets to their model. *)

include Fixpoint.Registry

let get_entrypoints models =
  let collect_targets ~target ~model target_accumulator =
    let { Model.modes; _ } = model in
    if Model.ModeSet.contains Model.Mode.Entrypoint modes then
      target :: target_accumulator
    else
      target_accumulator
  in
  fold ~init:[] ~f:collect_targets models
