(** Copyright 2016-present Facebook. All rights reserved. **)

type t = float

let start () = Unix.gettimeofday ()


let stop start_time =
  let stop_time = Unix.gettimeofday () in
  stop_time -. start_time


let stop_in_ms start_time =
  let stop_time = Unix.gettimeofday () in
  (stop_time -. start_time) *. 1000.0 |> Core.Int.of_float
