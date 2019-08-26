(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core

type t = {
  add_raw_source: Source.t -> unit;
  add_source: Source.t -> unit;
  remove_sources: Reference.t list -> unit;
  update_raw_and_compute_dependencies:
    update:(unit -> unit) -> Reference.t list -> Reference.t list;
  update_and_compute_dependencies: update:(unit -> unit) -> Reference.t list -> Reference.t list;
  get_raw_source: ?dependency:Reference.t -> Reference.t -> Source.t option;
  get_raw_wildcard_exports: ?dependency:Reference.t -> Reference.t -> Reference.t list option;
  get_source: ?dependency:Reference.t -> Reference.t -> Source.t option;
  get_wildcard_exports: ?dependency:Reference.t -> Reference.t -> Reference.t list option;
  get_source_path: Reference.t -> SourcePath.t option;
}

module RawSourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "Unprocessed source"

  let compare left right = Int.compare (Source.hash left) (Source.hash right)

  let unmarshall value = Marshal.from_string value 0
end

module RawSources =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (RawSourceValue)

module RawWildcardExportsValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Unprocessed wildcard exports"

  let compare = List.compare Reference.compare

  let unmarshall value = Marshal.from_string value 0
end

module RawWildcardExports =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (RawWildcardExportsValue)

module SourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "AST"

  let compare left right = Int.compare (Source.hash left) (Source.hash right)

  let unmarshall value = Marshal.from_string value 0
end

module Sources =
  Memory.DependencyTrackedTableNoCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (SourceValue)

module WildcardExportsValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Wildcard exports"

  let compare = List.compare Reference.compare

  let unmarshall value = Marshal.from_string value 0
end

module WildcardExports =
  Memory.DependencyTrackedTableWithCache
    (SharedMemoryKeys.ReferenceKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (WildcardExportsValue)

let create module_tracker =
  let add_raw_source ({ Source.qualifier; _ } as source) =
    RawSources.add qualifier source;
    RawWildcardExports.write_through qualifier (Source.wildcard_exports_of source)
  in
  let add_source ({ Source.qualifier; _ } as source) =
    Sources.add qualifier source;
    WildcardExports.write_through qualifier (Source.wildcard_exports_of source)
  in
  let remove_sources qualifiers =
    let keys = Sources.KeySet.of_list qualifiers in
    RawSources.remove_batch keys;
    Sources.remove_batch keys;
    RawWildcardExports.remove_batch keys;
    WildcardExports.remove_batch keys
  in
  let update_raw_and_compute_dependencies ~update qualifiers =
    let keys = RawSources.KeySet.of_list qualifiers in
    let (), dependency_set =
      SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
      |> RawSources.add_to_transaction ~keys
      |> RawWildcardExports.add_to_transaction ~keys
      |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
    in
    List.fold qualifiers ~init:dependency_set ~f:(fun sofar qualifier ->
        SharedMemoryKeys.ReferenceDependencyKey.KeySet.add qualifier sofar)
    |> SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements
  in
  let update_and_compute_dependencies ~update qualifiers =
    let keys = Sources.KeySet.of_list qualifiers in
    let (), dependency_set =
      SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
      |> Sources.add_to_transaction ~keys
      |> WildcardExports.add_to_transaction ~keys
      |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
    in
    List.fold qualifiers ~init:dependency_set ~f:(fun sofar qualifier ->
        SharedMemoryKeys.ReferenceDependencyKey.KeySet.add qualifier sofar)
    |> SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements
  in
  {
    add_raw_source;
    add_source;
    remove_sources;
    update_raw_and_compute_dependencies;
    update_and_compute_dependencies;
    get_raw_source = RawSources.get;
    get_raw_wildcard_exports = RawWildcardExports.get;
    get_source = Sources.get;
    get_wildcard_exports = WildcardExports.get;
    get_source_path = ModuleTracker.lookup module_tracker;
  }


module Raw = struct
  let add_source { add_raw_source; _ } = add_raw_source

  let update_and_compute_dependencies { update_raw_and_compute_dependencies; _ } =
    update_raw_and_compute_dependencies


  let get_source { get_raw_source; _ } = get_raw_source

  let get_wildcard_exports { get_raw_wildcard_exports; _ } = get_raw_wildcard_exports
end

let add_source { add_source; _ } = add_source

let remove_sources { remove_sources; _ } = remove_sources

let update_and_compute_dependencies { update_and_compute_dependencies; _ } =
  update_and_compute_dependencies


let get_source { get_source; _ } = get_source

let get_wildcard_exports { get_wildcard_exports; _ } = get_wildcard_exports

let get_source_path { get_source_path; _ } = get_source_path

(* Both `load` and `store` are no-ops here since `Sources` and `WildcardExports` are in shared
   memory, and `Memory.load_shared_memory`/`Memory.save_shared_memory` will take care of the
   (de-)serialization for us. *)
let store _ = ()

let load = create

let shared_memory_hash_to_key_map qualifiers =
  let extend_map map ~new_map =
    Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
  in
  RawSources.compute_hashes_to_keys ~keys:qualifiers
  |> extend_map ~new_map:(RawWildcardExports.compute_hashes_to_keys ~keys:qualifiers)
  |> extend_map ~new_map:(Sources.compute_hashes_to_keys ~keys:qualifiers)
  |> extend_map ~new_map:(WildcardExports.compute_hashes_to_keys ~keys:qualifiers)


let serialize_decoded decoded =
  match decoded with
  | RawSources.Decoded (key, value) ->
      Some (SourceValue.description, Reference.show key, Option.map value ~f:Source.show)
  | RawWildcardExports.Decoded (key, value) ->
      Some
        ( WildcardExportsValue.description,
          Reference.show key,
          Option.map value ~f:(List.to_string ~f:Reference.show) )
  | Sources.Decoded (key, value) ->
      Some (SourceValue.description, Reference.show key, Option.map value ~f:Source.show)
  | WildcardExports.Decoded (key, value) ->
      Some
        ( WildcardExportsValue.description,
          Reference.show key,
          Option.map value ~f:(List.to_string ~f:Reference.show) )
  | _ -> None


let decoded_equal first second =
  match first, second with
  | RawSources.Decoded (_, first), RawSources.Decoded (_, second) ->
      Some (Option.equal Source.equal first second)
  | RawWildcardExports.Decoded (_, first), RawWildcardExports.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | Sources.Decoded (_, first), Sources.Decoded (_, second) ->
      Some (Option.equal Source.equal first second)
  | WildcardExports.Decoded (_, first), WildcardExports.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | _ -> None


type environment_t = t

module ReadOnly = struct
  type t = {
    get_source: Reference.t -> Source.t option;
    get_wildcard_exports: Reference.t -> Reference.t list option;
    get_source_path: Reference.t -> SourcePath.t option;
  }

  let create
      ?(get_source = fun _ -> None)
      ?(get_wildcard_exports = fun _ -> None)
      ?(get_source_path = fun _ -> None)
      ()
    =
    { get_source; get_wildcard_exports; get_source_path }


  let get_source { get_source; _ } = get_source

  let get_source_path { get_source_path; _ } = get_source_path

  let get_wildcard_exports { get_wildcard_exports; _ } = get_wildcard_exports

  let get_relative read_only qualifier =
    let open Option in
    get_source_path read_only qualifier >>| fun { SourcePath.relative; _ } -> relative
end

let read_only { get_source; get_wildcard_exports; get_source_path; _ } =
  { ReadOnly.get_source; get_wildcard_exports; get_source_path }
