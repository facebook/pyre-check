(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 *)

type counter

external counter_new : unit -> counter = "counter_new"

external counter_inc : counter -> unit = "counter_inc"

external counter_read : counter -> int = "counter_read"

let () = begin
  print_endline "[counter_test][info]: start";
  let counter = counter_new () in
  assert (counter_read counter == 0);
  counter_inc counter;
  assert (counter_read counter == 1);
  print_endline "[counter_test][info]: finish";
end
