(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open TaintDomains


type t = {
  forward: ForwardState.t;
  backward: BackwardState.t;
  taint_in_taint_out: BackwardState.t;
}
[@@deriving show]

val create: unit -> t
