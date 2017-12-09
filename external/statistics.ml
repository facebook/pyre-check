(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


let performance ?flush:_ ~name:_ ~time:_ ~labels:_ = Ok ()

let coverage ?flush:_ ~coverage:_ ~labels:_ = Ok ()
