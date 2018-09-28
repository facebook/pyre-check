(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

include module type of Hack_parallel.Std.SharedMem
  with type handle = Hack_parallel.Std.SharedMem.handle


val get_heap_handle: Configuration.Analysis.t -> handle

val worker_garbage_control: Gc.control

val report_statistics: unit -> unit

val save_shared_memory: path: string -> unit
val load_shared_memory: path: string -> unit
