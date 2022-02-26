(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

      let prefix = Prefix.make ()

      let description = "ChangedPathsHash"

      let unmarshall value = Marshal.from_string value 0
    end)

let save_current_paths ~scheduler ~configuration ~module_tracker =
  let save_paths _ source_paths =
    let save_path ({ SourcePath.qualifier; _ } as source_path) =
      let hash =
        SourcePath.full_path ~configuration source_path |> File.create |> HashResult.from_file
      in
      SharedMemoryHashes.remove_batch (SharedMemoryHashes.KeySet.singleton qualifier);
      SharedMemoryHashes.add qualifier hash
    in
    List.iter ~f:save_path source_paths
  in
  Scheduler.map_reduce
    scheduler
    ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
    ~initial:()
    ~map:save_paths
    ~reduce:(fun () () -> ())
    ~inputs:(ModuleTracker.source_paths module_tracker)
    ()


(* Used for removed path detection *)
module IndexedRelativePath = struct
  module T = struct
    type t = int * string [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

let compute_locally_changed_paths ~scheduler ~configuration ~old_module_tracker ~new_module_tracker =
  let timer = Timer.start () in
  let changed_paths changed new_source_paths =
    let changed_path ({ SourcePath.qualifier; _ } as source_path) =
      let old_hash = SharedMemoryHashes.get qualifier in
      let path = SourcePath.full_path ~configuration source_path in
      let current_hash = HashResult.from_file (File.create path) in
      if Option.equal HashResult.equal old_hash (Some current_hash) then
        None
      else
        Some path
    in
    changed @ List.filter_map new_source_paths ~f:changed_path
  in
  let changed_paths =
    Scheduler.map_reduce
      scheduler
      ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
      ~initial:[]
      ~map:changed_paths
      ~reduce:( @ )
      ~inputs:(ModuleTracker.source_paths new_module_tracker)
      ()
  in
  let removed_paths =
    let tracked_set =
      ModuleTracker.all_source_paths new_module_tracker
      |> List.map ~f:(fun { SourcePath.priority; relative; _ } -> priority, relative)
      |> IndexedRelativePath.Hash_set.of_list
    in
    ModuleTracker.all_source_paths old_module_tracker
    |> List.filter ~f:(fun { SourcePath.priority; relative; _ } ->
           let key = priority, relative in
           not (Hash_set.mem tracked_set key))
    |> List.map ~f:(SourcePath.full_path ~configuration)
  in
  Statistics.performance ~name:"computed locally changed files" ~timer ();
  changed_paths @ removed_paths
