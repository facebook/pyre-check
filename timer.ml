(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t = float

let start () = Unix.gettimeofday ()


let stop start_time =
  let stop_time = Unix.gettimeofday () in
  stop_time -. start_time


let stop_in_ms start_time =
  let stop_time = Unix.gettimeofday () in
  (stop_time -. start_time) *. 1000.0 |> Core.Int.of_float
