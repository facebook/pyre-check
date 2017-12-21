(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


let performance ?flush:_ ~name:_ ~time:_ ~normals:_ = Ok ()

let coverage ?flush:_ ~coverage:_ ~normals:_ = Ok ()

let event ?flush:_ ~name:_ ~integers:_ ~normals_ = ()
