(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ChangedPaths: implements logic that checks whether a given set of files have
 * changed between two runs. This is mostly used to implement caching in the
 * taint analysis.
 *)

open Core
open Ast
open Analysis

module HashResult = struct
  type t =
    | Hash of int
    | ReadError
  [@@deriving equal]

  let from_file file =
    match File.hash file with
    | Some hash -> Hash hash
    | None -> ReadError
end

(* Store the hash for each module in the shared memory. *)
module SharedMemoryHashes =
  Memory.NoCache.Make
    (SharedMemoryKeys.ReferenceKey)
    (struct
      type t = HashResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "ChangedPathsHash"
    end)

let hash_of_content analysis_path =
  ArtifactPath.raw analysis_path |> File.create |> HashResult.from_file


let save_current_paths ~scheduler ~configuration ~module_tracker =
  let save_paths module_paths =
    let save_path module_path =
      let hash = ModulePath.full_path ~configuration module_path |> hash_of_content in
      let qualifier = ModulePath.qualifier module_path in
      SharedMemoryHashes.remove_batch (SharedMemoryHashes.KeySet.singleton qualifier);
      SharedMemoryHashes.add qualifier hash
    in
    List.iter ~f:save_path module_paths
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:100
         ~preferred_chunks_per_worker:1
         ())
    ~initial:()
    ~map:save_paths
    ~reduce:(fun () () -> ())
    ~inputs:(ModuleTracker.module_paths module_tracker)
    ()


(* Used for removed path detection *)
module IndexedRelativePath = struct
  module T = struct
    type t = ModulePath.Raw.t [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

(* Return the list of paths to files that have changed between now and the previous call to
   `save_current_paths`. *)
let compute_locally_changed_paths ~scheduler ~configuration ~old_module_tracker ~new_module_tracker =
  let timer = Timer.start () in
  let changed_paths new_module_paths =
    let changed_path module_path =
      let old_hash = SharedMemoryHashes.get (ModulePath.qualifier module_path) in
      let path = ModulePath.full_path ~configuration module_path in
      let current_hash = hash_of_content path in
      if Option.equal HashResult.equal old_hash (Some current_hash) then
        None
      else
        Some path
    in
    List.filter_map new_module_paths ~f:changed_path
  in
  let changed_paths =
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:1
           ())
      ~initial:[]
      ~map:changed_paths
      ~reduce:( @ )
      ~inputs:(ModuleTracker.module_paths new_module_tracker)
      ()
  in
  let removed_paths =
    let tracked_set =
      ModuleTracker.all_module_paths new_module_tracker
      |> List.map ~f:ModulePath.raw
      |> IndexedRelativePath.Hash_set.of_list
    in
    ModuleTracker.all_module_paths old_module_tracker
    |> List.filter ~f:(fun module_path ->
           let key = ModulePath.raw module_path in
           not (Hash_set.mem tracked_set key))
    |> List.map ~f:(ModulePath.full_path ~configuration)
  in
  Statistics.performance ~name:"computed locally changed files" ~timer ();
  changed_paths @ removed_paths
