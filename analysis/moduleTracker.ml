(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Ast
open Pyre

module Lookup = struct
  type t =
    | Explicit of Ast.SourcePath.t
    | Implicit of Ast.Reference.t
  [@@deriving sexp, compare, eq]
end

type t = {
  module_to_files: SourcePath.t list Reference.Table.t;
  submodule_refcounts: int Reference.Table.t;
}

let insert_source_path ~configuration ~inserted existing_files =
  let rec insert sofar = function
    | [] -> List.rev_append sofar [inserted]
    | current_file :: rest as existing -> (
      match SourcePath.same_module_compare ~configuration inserted current_file with
      | 0 ->
          (* We have the following precondition for files that are in the same module: *)
          (* `same_module_compare a b = 0` implies `equal a b` *)
          assert (SourcePath.equal inserted current_file);

          (* Duplicate entry detected. Do nothing *)
          existing_files
      | x when x > 0 -> List.rev_append sofar (inserted :: existing)
      | _ -> insert (current_file :: sofar) rest )
  in
  insert [] existing_files


let remove_source_path ~configuration ~removed existing_files =
  let rec remove sofar = function
    | [] -> existing_files
    | current_file :: rest -> (
      match SourcePath.same_module_compare ~configuration removed current_file with
      | 0 ->
          let () =
            (* For removed files, we only check for equality on relative path & priority. *)
            (* There's a corner case (where symlink is involved) that may cause `removed` to have a
               different `is_external` flag. *)
            let partially_equal
                { SourcePath.relative = left_relative; priority = left_priority; _ }
                { SourcePath.relative = right_relative; priority = right_priority; _ }
              =
              String.equal left_relative right_relative && Int.equal left_priority right_priority
            in
            assert (partially_equal removed current_file)
          in
          List.rev_append sofar rest
      | x when x > 0 -> existing_files
      | _ -> remove (current_file :: sofar) rest )
  in
  remove [] existing_files


let find_files { Configuration.Analysis.local_root; search_path; excludes; extensions; _ } =
  let visited_directories = String.Hash_set.create () in
  let visited_files = String.Hash_set.create () in
  let valid_suffixes = ".py" :: ".pyi" :: extensions in
  let mark_visited set path =
    match Hash_set.strict_add set path with
    | Result.Ok () -> false
    | _ -> true
  in
  let directory_filter path =
    (* Do not scan excluding directories to speed up the traversal *)
    (not (List.exists excludes ~f:(fun regexp -> Str.string_match regexp path 0)))
    && not (mark_visited visited_directories path)
  in
  let file_filter path =
    let extension =
      Filename.split_extension path
      |> snd
      >>| (fun extension -> "." ^ extension)
      |> Option.value ~default:""
    in
    List.exists ~f:(String.equal extension) valid_suffixes && not (mark_visited visited_files path)
  in
  let search_roots = local_root :: List.map ~f:SearchPath.to_path search_path in
  List.map search_roots ~f:(fun root -> Path.list ~file_filter ~directory_filter ~root ())
  |> List.concat


let create_module_to_files configuration =
  let module_to_files = Reference.Table.create () in
  let process_file path =
    match SourcePath.create ~configuration path with
    | None -> ()
    | Some ({ SourcePath.qualifier; _ } as source_path) ->
        let update_table = function
          | None -> [source_path]
          | Some source_paths ->
              insert_source_path ~configuration ~inserted:source_path source_paths
        in
        Hashtbl.update module_to_files qualifier ~f:update_table
  in
  let files = find_files configuration in
  List.iter files ~f:process_file;
  module_to_files


let create_submodule_refcounts module_to_files =
  let submodule_refcounts = Reference.Table.create () in
  let process_module qualifier =
    let rec process_submodule = function
      | None -> ()
      | Some qualifier when Reference.is_empty qualifier -> ()
      | Some qualifier ->
          Reference.Table.update submodule_refcounts qualifier ~f:(function
              | None -> 1
              | Some count -> count + 1);
          process_submodule (Reference.prefix qualifier)
    in
    process_submodule (Some qualifier)
  in
  Hashtbl.keys module_to_files |> List.iter ~f:process_module;
  submodule_refcounts


let create configuration =
  let module_to_files = create_module_to_files configuration in
  let submodule_refcounts = create_submodule_refcounts module_to_files in
  { module_to_files; submodule_refcounts }


let lookup_source_path { module_to_files; _ } module_name =
  match Hashtbl.find module_to_files module_name with
  | Some (source_path :: _) -> Some source_path
  | _ -> None


let lookup_path ~configuration tracker path =
  SourcePath.create ~configuration path
  >>= fun { SourcePath.relative; priority; qualifier; _ } ->
  lookup_source_path tracker qualifier
  >>= fun ( { SourcePath.relative = tracked_relative; priority = tracked_priority; _ } as
          source_path ) ->
  if String.equal relative tracked_relative && Int.equal priority tracked_priority then
    Some source_path
  else
    None


let lookup { module_to_files; submodule_refcounts } module_name =
  match Hashtbl.find module_to_files module_name with
  | Some (source_path :: _) -> Some (Lookup.Explicit source_path)
  | _ -> (
    match Hashtbl.mem submodule_refcounts module_name with
    | true -> Some (Lookup.Implicit module_name)
    | false -> None )


let source_paths { module_to_files; _ } =
  Hashtbl.data module_to_files |> List.filter_map ~f:List.hd


let all_source_paths { module_to_files; _ } = Hashtbl.data module_to_files |> List.concat

let tracked_explicit_modules tracker =
  source_paths tracker |> List.map ~f:(fun { SourcePath.qualifier; _ } -> qualifier)


let is_module_tracked { module_to_files; submodule_refcounts } qualifier =
  Hashtbl.mem module_to_files qualifier || Hashtbl.mem submodule_refcounts qualifier


let explicit_module_count { module_to_files; _ } = Hashtbl.length module_to_files

module FileSystemEvent = struct
  type t =
    | Update of Path.t
    | Remove of Path.t

  let create path = if Path.file_exists path then Update path else Remove path
end

module IncrementalUpdate = struct
  type t =
    | New of SourcePath.t
    | Delete of Reference.t
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]
end

let update ~configuration ~paths { module_to_files; _ } =
  (* Process a single filesystem event *)
  let process_filesystem_event ~configuration tracker = function
    | FileSystemEvent.Update path -> (
      match SourcePath.create ~configuration path with
      | None ->
          Log.warning "`%a` not found in search path." Path.pp path;
          None
      | Some ({ SourcePath.qualifier; _ } as source_path) -> (
        match Hashtbl.find tracker qualifier with
        | None ->
            (* New file for a new module *)
            Hashtbl.set tracker ~key:qualifier ~data:[source_path];
            Some (IncrementalUpdate.New source_path)
        | Some source_paths ->
            let new_source_paths =
              insert_source_path ~configuration ~inserted:source_path source_paths
            in
            let new_source_path = List.hd_exn new_source_paths in
            Hashtbl.set tracker ~key:qualifier ~data:new_source_paths;
            if SourcePath.equal new_source_path source_path then
              (* Updating a shadowing file means the module gets changed *)
              Some (IncrementalUpdate.New source_path)
            else (* Updating a shadowed file should not trigger any reanalysis *)
              None ) )
    | FileSystemEvent.Remove path -> (
      match SourcePath.create ~configuration path with
      | None ->
          Log.warning "`%a` not found in search path." Path.pp path;
          None
      | Some ({ SourcePath.qualifier; _ } as source_path) -> (
          Hashtbl.find tracker qualifier
          >>= fun source_paths ->
          match source_paths with
          | [] ->
              (* This should never happen but handle it just in case *)
              Hashtbl.remove tracker qualifier;
              None
          | old_source_path :: _ -> (
            match remove_source_path ~configuration ~removed:source_path source_paths with
            | [] ->
                (* Last remaining file for the module gets removed. *)
                Hashtbl.remove tracker qualifier;
                Some (IncrementalUpdate.Delete qualifier)
            | new_source_path :: _ as new_source_paths ->
                Hashtbl.set tracker ~key:qualifier ~data:new_source_paths;
                if SourcePath.equal old_source_path new_source_path then
                  (* Removing a shadowed file should not trigger any reanalysis *)
                  None
                else (* Removing source_path un-shadows another source file. *)
                  Some (IncrementalUpdate.New new_source_path) ) ) )
  in
  (* Make sure we have only one update per module *)
  let merge_updates updates =
    let table = Reference.Table.create () in
    let process_update update =
      match update with
      | IncrementalUpdate.New { SourcePath.qualifier; _ } ->
          Hashtbl.set table ~key:qualifier ~data:update
      | IncrementalUpdate.Delete qualifier ->
          let update = function
            | None
            | Some (IncrementalUpdate.New _) ->
                Some update
            | Some (IncrementalUpdate.Delete _) ->
                let message =
                  Format.asprintf "Illegal state: double delete module %a" Reference.pp qualifier
                in
                failwith message
          in
          Hashtbl.change table qualifier ~f:update
    in
    List.iter updates ~f:process_update;
    Hashtbl.data table
  in
  (* Since `process_filesystem_event` is not idempotent, we don't want duplicated filesystem events *)
  List.dedup_and_sort ~compare:Path.compare paths
  |> List.map ~f:FileSystemEvent.create
  |> List.filter_map ~f:(process_filesystem_event ~configuration module_to_files)
  |> merge_updates


module SharedMemory = Memory.Serializer (struct
  type nonrec t = t

  module Serialized = struct
    type t = (Reference.t * SourcePath.t list) list * (Reference.t * int) list

    let prefix = Prefix.make ()

    let description = "Module tracker"

    let unmarshall value = Marshal.from_string value 0
  end

  let serialize { module_to_files; submodule_refcounts } =
    Hashtbl.to_alist module_to_files, Hashtbl.to_alist submodule_refcounts


  let deserialize (module_data, submodule_data) =
    {
      module_to_files = Reference.Table.of_alist_exn module_data;
      submodule_refcounts = Reference.Table.of_alist_exn submodule_data;
    }
end)
