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


let initialize log_level =
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
          hash_table_pow = 22;
          shm_dirs = ["/dev/shm"; "/pyre"];
          shm_min_avail = 1024 * 1024 * 512; (* 512 MB *)
          log_level;
        } in
      let heap_handle = SharedMemory.init shared_mem_config in
      configuration := Some { heap_handle; minor_heap_size };
      { heap_handle; minor_heap_size }
  | Some configuration ->
      configuration


let get_heap_handle { Configuration.Analysis.debug; _ } =
  let log_level =
    if debug then
      1
    else
      0
  in
  let { heap_handle; _ } = initialize log_level in
  heap_handle


let report_statistics () =
  Measure.print_stats ();
  Measure.print_distributions ()


let save_shared_memory ~path =
  collect `aggressive;
  SharedMem.save_table path


let load_shared_memory ~path =
  SharedMem.load_table path


let unsafe_little_endian_representation ~key =
  (* Ensure that key is a well-formed digest. *)
  Digest.to_hex key
  |> Digest.from_hex
  |> fun digest -> assert (Digest.equal digest key);
  (* Mimic what hack_parallel does, which is cast a key to a uint64_t pointer and dereference.
     This code is not portable by any means. *)
  let rec compute_little_endian accumulator index =
    let accumulator =
      Int64.mul accumulator (Int64.of_int 256)
      |> Int64.add (Int64.of_int (Char.code key.[index]))
    in
    if index = 0 then
      accumulator
    else
      compute_little_endian accumulator (index - 1)
  in
  (* Take the first 8 bytes in reverse order. *)
  compute_little_endian Int64.zero 7
