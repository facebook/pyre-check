(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hack_parallel.Std
module Daemon = Daemon
open Core

let is_master () = Int.equal 0 (Worker.current_worker_id ())

type t =
  | SequentialScheduler
  | ParallelScheduler of Worker.t list

module Policy = struct
  type t = number_of_workers:int -> number_of_tasks:int -> int

  let divide_work ~number_of_workers ~number_of_tasks policy =
    policy ~number_of_workers ~number_of_tasks


  let legacy_fixed_chunk_size chunk_size =
    assert (chunk_size > 0);
    fun ~number_of_workers ~number_of_tasks ->
      Core.Int.max number_of_workers ((number_of_tasks / chunk_size) + 1)


  let legacy_fixed_chunk_count () ~number_of_workers ~number_of_tasks =
    Core.Int.max
      number_of_workers
      (let chunk_multiplier = Core.Int.min 10 (1 + (number_of_tasks / 400)) in
       number_of_workers * chunk_multiplier)


  let fixed_chunk_size ?minimum_chunk_size ~minimum_chunks_per_worker ~preferred_chunk_size () =
    let minimum_chunk_size = Option.value minimum_chunk_size ~default:preferred_chunk_size in
    assert (minimum_chunk_size >= 0);
    assert (preferred_chunk_size >= minimum_chunk_size);
    assert (minimum_chunks_per_worker >= 0);
    fun ~number_of_workers ~number_of_tasks ->
      let preferred_chunk_count = (number_of_tasks / preferred_chunk_size) + 1 in
      let minimum_chunk_count = minimum_chunks_per_worker * number_of_workers in
      if preferred_chunk_count >= minimum_chunk_count then
        preferred_chunk_count
      else
        let fallback_chunk_size = number_of_tasks / minimum_chunk_count in
        if fallback_chunk_size >= minimum_chunk_size then
          minimum_chunk_count
        else
          1


  let fixed_chunk_count
      ?minimum_chunks_per_worker
      ~minimum_chunk_size
      ~preferred_chunks_per_worker
      ()
    =
    let minimum_chunks_per_worker =
      Option.value minimum_chunks_per_worker ~default:preferred_chunks_per_worker
    in
    assert (minimum_chunks_per_worker >= 0);
    assert (preferred_chunks_per_worker >= minimum_chunks_per_worker);
    assert (minimum_chunks_per_worker >= 0);
    fun ~number_of_workers ~number_of_tasks ->
      let preferred_chunk_count = preferred_chunks_per_worker * number_of_workers in
      let preferred_chunk_size = number_of_tasks / preferred_chunk_count in
      if preferred_chunk_size >= minimum_chunk_size then
        preferred_chunk_count
      else
        let minimum_chunk_count = minimum_chunks_per_worker * number_of_workers in
        let fallback_chunk_count = (number_of_tasks / minimum_chunk_size) + 1 in
        if fallback_chunk_count >= minimum_chunk_count then
          fallback_chunk_count
        else
          1
end

let entry =
  Worker.register_entry_point ~restore:(fun (log_state, profiling_state, statistics_state) ->
      Log.GlobalState.restore log_state;
      Profiling.GlobalState.restore profiling_state;
      Statistics.GlobalState.restore statistics_state;
      ())


let create
    ~configuration:({ Configuration.Analysis.parallel; number_of_workers; _ } as configuration)
    ()
  =
  let heap_handle = Memory.get_heap_handle configuration in
  if parallel then
    let workers =
      Hack_parallel.Std.Worker.make
        ~saved_state:
          ( (* These states need to be restored in the worker process. *)
            Log.GlobalState.get (),
            Profiling.GlobalState.get (),
            Statistics.GlobalState.get () )
        ~entry
        ~nbr_procs:number_of_workers
        ~heap_handle
        ~gc_control:Memory.worker_garbage_control
    in
    ParallelScheduler workers
  else
    SequentialScheduler


let create_sequential () = SequentialScheduler

let destroy = function
  | SequentialScheduler -> ()
  | ParallelScheduler workers -> List.iter workers ~f:Worker.kill


let with_scheduler ~configuration ~f =
  let scheduler = create ~configuration () in
  match scheduler with
  | SequentialScheduler -> f SequentialScheduler
  | ParallelScheduler _ -> Exn.protectx ~f scheduler ~finally:(fun _ -> destroy scheduler)


let run_process process =
  let result = process () in
  Statistics.flush ();
  result


let map_reduce scheduler ~policy ~initial ~map ~reduce ~inputs () =
  let sequential_map_reduce () = map initial inputs |> fun mapped -> reduce mapped initial in
  match scheduler with
  | ParallelScheduler workers ->
      let number_of_workers = List.length workers in
      let number_of_chunks =
        Policy.divide_work ~number_of_workers ~number_of_tasks:(List.length inputs) policy
      in
      if number_of_chunks = 1 then
        sequential_map_reduce ()
      else
        let map accumulator inputs = (fun () -> map accumulator inputs) |> run_process in
        MultiWorker.call
          (Some workers)
          ~job:map
          ~merge:reduce
          ~neutral:initial
          ~next:(Bucket.make ~num_workers:number_of_chunks inputs)
  | SequentialScheduler -> sequential_map_reduce ()


let iter scheduler ~policy ~f ~inputs =
  map_reduce
    scheduler
    ~policy
    ~initial:()
    ~map:(fun _ inputs -> f inputs)
    ~reduce:(fun _ _ -> ())
    ~inputs
    ()


let is_parallel = function
  | SequentialScheduler -> false
  | ParallelScheduler _ -> true


let once_per_worker scheduler ~configuration:_ ~f =
  match scheduler with
  | SequentialScheduler -> ()
  | ParallelScheduler workers ->
      let handles = List.map workers ~f:(fun worker -> Worker.call worker f ()) in
      let rec collect_all = function
        | [] -> ()
        | handles ->
            let { Worker.readys; waiters } = Worker.select handles in
            List.iter readys ~f:Worker.get_result;
            collect_all waiters
      in
      collect_all handles
