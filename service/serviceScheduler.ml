(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel.Std

module SharedMemory = SharedMem


type t = {
  is_parallel: bool;
  workers: Worker.t list;
  bucket_multiplier: int;
}


let default_temporary_dir =
  "/pyre"


let default_shm_dirs =
  [ "/dev/shm"; default_temporary_dir ]


let entry =
  Worker.register_entry_point ~restore:(fun _ -> ())


let number_of_processes =
  20


let gc_control =
  Gc.get ()


let map_reduce { workers; bucket_multiplier; _ } ~init ~map ~reduce work =
  let bucket_multiplier = Core.Int.min bucket_multiplier (1 + (List.length work / 400)) in
  MultiWorker.call (Some workers)
    ~job:map
    ~merge:reduce
    ~neutral:init
    ~next:(Bucket.make ~num_workers:(number_of_processes * bucket_multiplier) work)


let rec wait_until_ready handle =
  let { Worker.readys; _ } = Worker.select [handle] in
  match readys with
  | [] -> wait_until_ready handle
  | ready :: _ ->
      ready


let single_job { workers; _ } ~f work =
  match workers with
  | worker::_ ->
      Worker.call worker f work
      |> wait_until_ready
      |> Worker.get_result
  | [] -> failwith "This service contains no workers"



module Memory = struct
  type bytes = int

  type configuration = {
    heap_handle: SharedMemory.handle;
    minor_heap_size: bytes;
  }

  let configuration: configuration option ref = ref None

  let initialize () =
    match !configuration with
    | None ->
        let minor_heap_size = 2 * 1024 * 1024 in (* 2 MB *)
        Gc.set { (Gc.get ()) with Gc.minor_heap_size };
        let shared_mem_config =
          let open SharedMemory in
          {
            global_size = 4096 * 1024 * 1024; (* 4096 MB *)
            heap_size = 4096 * 1024 * 1024; (* 4096 MB *)
            dep_table_pow = 19;
            hash_table_pow = 21;
            shm_dirs = default_shm_dirs;
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
end


let create ~is_parallel ?(bucket_multiplier = 10) () =
  let heap_handle = Memory.get_heap_handle () in
  let workers =
    Hack_parallel.Std.Worker.make
      ?call_wrapper:None
      ~saved_state:()
      ~entry
      ~nbr_procs:number_of_processes
      ~heap_handle
      ~gc_control
  in
  SharedMemory.connect heap_handle ~is_master:true;
  { workers; is_parallel; bucket_multiplier }


let mock () =
  Memory.initialize () |> ignore;
  { workers = []; is_parallel = false; bucket_multiplier = 1 }


let is_parallel { is_parallel; _ } = is_parallel


let with_parallel ~is_parallel service = { service with is_parallel }


let destroy _ = Worker.killall ()
