(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

include module type of Hack_parallel.Std.SharedMem
  with type handle = Hack_parallel.Std.SharedMem.handle


val get_heap_handle: unit -> handle

(* Between 0.0 and 1.0 *)
val heap_use_ratio: unit -> float
val slot_use_ratio: unit -> float

val worker_garbage_control: Gc.control
