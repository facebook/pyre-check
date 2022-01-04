(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

(* Used for removed path detection *)
module IndexedRelativePath = struct
  module T = struct
    type t = int * string [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

(* If we're analyzing generated code, Watchman will be blind to any changes to said code. In order
   to be safe, compute hashes for all files that a fresh Pyre run would analyze. *)
let compute_locally_changed_paths
    ~scheduler
    ~configuration
    ~module_tracker:old_module_tracker
    ~ast_environment
  =
  let timer = Timer.start () in
  let new_module_tracker = ModuleTracker.create configuration in
  let changed_paths changed new_source_paths =
    let changed_path ({ SourcePath.qualifier; _ } as source_path) =
      let old_hash =
        AstEnvironment.ReadOnly.get_raw_source ast_environment qualifier
        >>= function
        | Result.Ok { Source.metadata = { Source.Metadata.raw_hash; _ }; _ } -> Some raw_hash
        | Result.Error _ -> None
      in
      let path = SourcePath.full_path ~configuration source_path in
      let current_hash = File.hash (File.create path) in
      if Option.equal Int.equal old_hash current_hash then
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
