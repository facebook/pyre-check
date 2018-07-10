(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel.Std

module SharedMemory = SharedMem


type t = {
  is_parallel: bool;
  workers: Worker.t list;
  number_of_workers: int;
  bucket_multiplier: int;
}


module Memory = struct
  type bytes = int

  type configuration = {
    heap_handle: SharedMemory.handle;
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
end


let entry =
  Worker.register_entry_point ~restore:(fun _ -> ())


let create
    ~configuration:{ Configuration.parallel; number_of_workers; _ }
    ?(bucket_multiplier = 10)
    () =
  let heap_handle = Memory.get_heap_handle () in
  let workers =
    Hack_parallel.Std.Worker.make
      ?call_wrapper:None
      ~saved_state:()
      ~entry
      ~nbr_procs:number_of_workers
      ~heap_handle
      ~gc_control:Memory.worker_garbage_control
  in
  SharedMemory.connect heap_handle ~is_master:true;
  { workers; number_of_workers; bucket_multiplier; is_parallel = parallel }


let initialize_process ~configuration:({ Configuration.verbose; sections; _ } as configuration) =
  Log.initialize ~verbose ~sections;
  Configuration.set_global configuration


let map_reduce
    { workers; bucket_multiplier; number_of_workers; _ }
    ?bucket_size
    ~configuration
    ~init
    ~map
    ~reduce
    work =
  let number_of_workers =
    match bucket_size with
    | Some exact_size when exact_size > 0 ->
        (List.length work / exact_size) + 1
    | _ ->
        let bucket_multiplier = Core.Int.min bucket_multiplier (1 + (List.length work / 400)) in
        number_of_workers * bucket_multiplier
  in
  let map accumulator inputs =
    initialize_process ~configuration;
    map accumulator inputs
  in
  MultiWorker.call (Some workers)
    ~job:map
    ~merge:reduce
    ~neutral:init
    ~next:(Bucket.make ~num_workers:number_of_workers work)


let iter scheduler ~configuration ~f work =
  map_reduce
    scheduler
    ~configuration
    ~init:()
    ~map:(fun _ work -> f work)
    ~reduce:(fun _ _ -> ())
    work


let single_job { workers; _ } ~f work =
  let rec wait_until_ready handle =
    let { Worker.readys; _ } = Worker.select [handle] in
    match readys with
    | [] -> wait_until_ready handle
    | ready :: _ -> ready
  in
  match workers with
  | worker::_ ->
      Worker.call worker f work
      |> wait_until_ready
      |> Worker.get_result
  | [] ->
      failwith "This service contains no workers"


let mock () =
  Memory.initialize () |> ignore;
  { workers = []; number_of_workers = 1; bucket_multiplier = 1; is_parallel = false }


let is_parallel { is_parallel; _ } =
  is_parallel


let with_parallel ~is_parallel service =
  { service with is_parallel }


let destroy _ =
  Worker.killall ()
