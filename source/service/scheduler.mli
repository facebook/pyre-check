(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hack_parallel.Std
module Daemon = Daemon

val is_master : unit -> bool

module Policy : sig
  type t

  val divide_work : number_of_workers:int -> number_of_tasks:int -> t -> int

  val legacy_fixed_chunk_size : int -> t

  val legacy_fixed_chunk_count : unit -> t

  (* Our workload distribution algorithm works like this: we take the input list of tasks, break it
     apart into smaller chunks, and put all the chunks into a queue. Then we repeatedly poll from a
     pool of workers -- if all workers are busy, we block and wait for one that becomes non-busy;
     otherwise, we pull a task out of the queue, assign it to a non-busy worker, and poll for the
     next non-busy worker. Specifying a scheduling policy, under this context, essentially boils
     down to deciding how the list of input tasks can be chunked. *)

  (* The fixed_chunk_size policy attempts to keep the size of each chunk constant. It also tries to
     make sure that the number of chunks does not fall below a given threshold. *)
  val fixed_chunk_size
    :  ?minimum_chunk_size:int ->
    minimum_chunks_per_worker:int ->
    preferred_chunk_size:int ->
    unit ->
    t

  (* The fixed_chunk_count policy attempts to keep the number of chunks constant. It also tries to
     make sure that the chunk size does not fall below a given threshold. *)
  val fixed_chunk_count
    :  ?minimum_chunks_per_worker:int ->
    minimum_chunk_size:int ->
    preferred_chunks_per_worker:int ->
    unit ->
    t
end

type t

val create : configuration:Configuration.Analysis.t -> unit -> t

val create_sequential : unit -> t

val destroy : t -> unit

val with_scheduler : configuration:Configuration.Analysis.t -> f:(t -> 'a) -> 'a

val run_process : (unit -> 'result) -> 'result

(* NOTE: If incremental check matters, and if within `map` new dependency keys are going to be
   created, you might want to use `SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce`
   instead. Otherwise, dependencies added in the workers will just get dropped. *)
val map_reduce
  :  t ->
  policy:Policy.t ->
  initial:'state ->
  map:('state -> 'input list -> 'intermediate) ->
  reduce:('intermediate -> 'state -> 'state) ->
  inputs:'input list ->
  unit ->
  'state

(* NOTE: If incremental check matters, and if within `f` new dependency keys are going to be
   created, you might want to use `SharedMemoryKeys.DependencyKey.Registry.collected_iter` instead. *)
val iter : t -> policy:Policy.t -> f:('input list -> unit) -> inputs:'input list -> unit

val is_parallel : t -> bool

val once_per_worker : t -> configuration:Configuration.Analysis.t -> f:(unit -> unit) -> unit
