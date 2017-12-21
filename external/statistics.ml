(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


let sample ?(system_time = 0.0) ?(integers = []) ?(normals = []) () =
  ignore system_time;
  ignore integers;
  ignore normals;
  ""

let flush () = ()

let performance ?flush:_ ~root:_ ~time:_ ~normals:_ = ()

let coverage ?flush:_ ~root:_ ~coverage:_ ~normals:_ = ()

let event ?flush:_ ~root:_ ~name:_ ~integers:_ ~normals:_ = ()
