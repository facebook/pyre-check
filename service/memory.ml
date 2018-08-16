(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module SharedMemory = Hack_parallel.Std.SharedMem

include SharedMemory


type bytes = int

type configuration = {
  heap_handle: Hack_parallel.Std.SharedMem.handle;
  minor_heap_size: bytes;
}


let configuration: configuration option ref = ref None


let initial_heap_size = 4096 * 1024 * 1024 (* 4 GB *)


let worker_garbage_control =
  {
    (Gc.get ()) with
    Gc.minor_heap_size = 256 * 1024; (* 256 KB *)
    space_overhead = 100;
  }


let initialize () =
  match !configuration with
  | None ->
      let minor_heap_size = 4 * 1024 * 1024 in (* 4 MB *)
      let space_overhead = 50 in
      (* Only sets the GC for the master process - the parallel
           workers use GC settings with less overhead. *)
      Gc.set {
        (Gc.get ()) with
        Gc.minor_heap_size;
        space_overhead;
      };
      let shared_mem_config =
        let open SharedMemory in
        {
          global_size = initial_heap_size;
          heap_size = initial_heap_size;
          dep_table_pow = 19;
          hash_table_pow = 21;
          shm_dirs = ["/dev/shm"; "/pyre"];
          shm_min_avail = 1024 * 1024 * 512; (* 512 MB *)
          log_level = 0;
        } in
      let heap_handle = SharedMemory.init shared_mem_config in
      configuration := Some { heap_handle; minor_heap_size };
      { heap_handle; minor_heap_size }
  | Some configuration ->
      configuration


let get_heap_handle () =
  let { heap_handle; _ } = initialize () in
  heap_handle


let heap_use_ratio () =
  Core.Float.of_int (SharedMem.heap_size ()) /.
  Core.Float.of_int initial_heap_size


let slot_use_ratio () =
  let { SharedMem.used_slots; slots; _ } = SharedMem.hash_stats () in
  Core.Float.of_int used_slots /. Core.Float.of_int slots
