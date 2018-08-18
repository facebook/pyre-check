(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel.Std

module Daemon = Daemon


type t = {
  is_parallel: bool;
  workers: Worker.t list;
  number_of_workers: int;
  bucket_multiplier: int;
}


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
  Memory.connect heap_handle ~is_master:true;
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
  MultiWorker.call
    (Some workers)
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
  Memory.get_heap_handle () |> ignore;
  { workers = []; number_of_workers = 1; bucket_multiplier = 1; is_parallel = false }


let is_parallel { is_parallel; _ } =
  is_parallel


let with_parallel ~is_parallel service =
  { service with is_parallel }


let workers { workers; _ } =
  workers


let destroy _ =
  Worker.killall ()
