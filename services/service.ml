(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel.Std

module SharedMemory = SharedMem


type t = {
  is_parallel: bool;
  workers: Worker.t list
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


let map_reduce { workers; _ } ~init ~map ~reduce work =
  MultiWorker.call (Some workers)
    ~job:map
    ~merge:reduce
    ~neutral:init
    ~next:(Bucket.make ~num_workers:number_of_processes work)


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


let heap_handle : SharedMemory.handle option ref = ref None


let initialize_heap_handle () =
  match !heap_handle with
  | None ->
      let shared_mem_config =
        let open SharedMemory in
        {
          global_size = 1024 * 1024 * 1024; (* 1024 MB *)
          heap_size = 1024 * 1024 * 1024; (* 1024 MB *)
          dep_table_pow = 17;
          hash_table_pow = 19;
          shm_dirs = default_shm_dirs;
          shm_min_avail = 1024 * 1024 * 512; (* 512 MB *)
          log_level = 0;
        } in
      let new_heap_handle = SharedMemory.init shared_mem_config in
      heap_handle := Some new_heap_handle;
      new_heap_handle
  | Some heap_handle -> heap_handle


let create ~is_parallel () =
  let heap_handle = initialize_heap_handle () in
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
  { workers; is_parallel }


let mock () =
  initialize_heap_handle () |> ignore;
  { workers = []; is_parallel = false }


let is_parallel { is_parallel; _ } = is_parallel


let with_parallel ~is_parallel service = { service with is_parallel }


let destroy _ = Worker.killall ()
