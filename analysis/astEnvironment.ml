(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Pyre

type t = {
  add_source: Source.t -> unit;
  remove_sources: Reference.t list -> unit;
  get_source: Reference.t -> Source.t option;
  get_wildcard_exports: Reference.t -> Reference.t list option;
  get_source_path: Reference.t -> SourcePath.t option;
}

module SourceValue = struct
  type t = Source.t

  let prefix = Prefix.make ()

  let description = "AST"

  let unmarshall value = Marshal.from_string value 0
end

module Sources = Memory.NoCache.Make (SharedMemoryKeys.ReferenceKey) (SourceValue)

module WildcardExportsValue = struct
  type t = Reference.t list

  let prefix = Prefix.make ()

  let description = "Wildcard exports"

  let unmarshall value = Marshal.from_string value 0
end

module WildcardExports =
  Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (WildcardExportsValue)

let add_wildcard_export { Source.qualifier; statements; _ } =
  let open Statement in
  let open Expression in
  let toplevel_public, dunder_all =
    let gather_toplevel (public_values, dunder_all) { Node.value; _ } =
      let filter_private =
        let is_public name =
          let dequalified =
            Reference.drop_prefix ~prefix:qualifier name |> Reference.sanitize_qualified
          in
          if not (String.is_prefix ~prefix:"_" (Reference.show dequalified)) then
            Some dequalified
          else
            None
        in
        List.filter_map ~f:is_public
      in
      match value with
      | Assign
          {
            Assign.target = { Node.value = Name (Name.Identifier target); _ };
            value = { Node.value = Expression.List names; _ };
            _;
          }
        when String.equal (Identifier.sanitized target) "__all__" ->
          let to_reference = function
            | { Node.value = Expression.String { value = name; _ }; _ } ->
                Reference.create name
                |> Reference.last
                |> (fun last -> if String.is_empty last then None else Some last)
                >>| Reference.create
            | _ -> None
          in
          public_values, Some (List.filter_map ~f:to_reference names)
      | Assign { Assign.target = { Node.value = Name target; _ }; _ }
        when Expression.is_simple_name target ->
          public_values @ filter_private [target |> Expression.name_to_reference_exn], dunder_all
      | Class { Class.name; _ } -> public_values @ filter_private [name], dunder_all
      | Define { Define.signature = { name; _ }; _ } ->
          public_values @ filter_private [name], dunder_all
      | Import { Import.imports; _ } ->
          let get_import_name { Import.alias; name } = Option.value alias ~default:name in
          public_values @ filter_private (List.map imports ~f:get_import_name), dunder_all
      | _ -> public_values, dunder_all
    in
    List.fold ~f:gather_toplevel ~init:([], None) statements
  in
  let wildcard_exports = Option.value dunder_all ~default:toplevel_public in
  WildcardExports.write_through qualifier wildcard_exports


let create module_tracker =
  {
    add_source =
      (fun ({ Source.qualifier; _ } as source) ->
        Sources.add qualifier source;
        add_wildcard_export source);
    remove_sources =
      (fun qualifiers ->
        let keys = Sources.KeySet.of_list qualifiers in
        Sources.remove_batch keys;
        WildcardExports.remove_batch keys);
    get_source = Sources.get;
    get_wildcard_exports = (fun qualifier -> WildcardExports.get qualifier);
    get_source_path = ModuleTracker.lookup module_tracker;
  }


let add_source { add_source; _ } = add_source

let remove_sources { remove_sources; _ } = remove_sources

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
  Sources.compute_hashes_to_keys ~keys:qualifiers
  |> extend_map ~new_map:(WildcardExports.compute_hashes_to_keys ~keys:qualifiers)


let serialize_decoded decoded =
  match decoded with
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
