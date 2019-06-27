(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Ast
open Pyre

module SourceFile = struct
  type t = {
    relative_path: Path.RelativePath.t;
    priority: int;
    is_stub: bool;
    is_external: bool;
    is_init: bool
  }
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]

  let pp formatter { relative_path; priority; is_stub; is_external; is_init } =
    let priority = Int.to_string priority in
    let is_stub = if is_stub then " [STUB]" else "" in
    let is_external = if is_external then " [EXTERNAL]" else "" in
    let is_init = if is_init then " [INIT]" else "" in
    Format.fprintf
      formatter
      "[%a(%s)%s%s%s]"
      Path.RelativePath.pp
      relative_path
      priority
      is_stub
      is_external
      is_init


  let qualifier { relative_path; _ } =
    let relative = Path.RelativePath.relative relative_path in
    (* TODO (T46153421): Purge `File.Handle.create_for_testing` from production code *)
    let handle = File.Handle.create_for_testing relative in
    Source.qualifier ~handle


  let create_from_search_path ~is_external ~search_path path =
    SearchPath.search_for_path ~search_path path
    >>= fun SearchPath.{ relative_path; priority } ->
    let path = Path.Relative relative_path in
    let is_stub = Path.is_python_stub path in
    let is_init = Path.is_python_init path in
    Some { relative_path; priority; is_stub; is_external; is_init }


  let should_type_check
      ~configuration:{ Configuration.Analysis.filter_directories; ignore_all_errors; _ }
      path
    =
    let directory_contains ~path directory = Path.directory_contains ~directory path in
    let filter_directories = Option.value filter_directories ~default:[] in
    let ignore_all_errors = Option.value ignore_all_errors ~default:[] in
    List.exists filter_directories ~f:(directory_contains ~path)
    && not (List.exists ignore_all_errors ~f:(directory_contains ~path))


  let create
      ~configuration:( { Configuration.Analysis.local_root; search_path; excludes; _ } as
                     configuration )
      path
    =
    let absolute_path = Path.absolute path in
    match List.exists excludes ~f:(fun regexp -> Str.string_match regexp absolute_path 0) with
    | true -> None
    | false ->
        let search_path = List.append search_path [SearchPath.Root local_root] in
        let is_external = not (should_type_check ~configuration path) in
        create_from_search_path ~is_external ~search_path path


  (* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
     only. Do NOT use it on aribitrary SourceFiles. *)
  let same_module_compare
      { priority = left_priority; is_stub = left_is_stub; is_init = left_is_init; _ }
      { priority = right_priority; is_stub = right_is_stub; is_init = right_is_init; _ }
    =
    (* Stub file always takes precedence *)
    match left_is_stub, right_is_stub with
    | true, false -> 1
    | false, true -> -1
    | _, _ -> (
      (* Smaller int means higher priority *)
      match Int.compare right_priority left_priority with
      | 0 -> (
        (* Package takes precedence over file module with the same name *)
        match left_is_init, right_is_init with
        | true, false -> 1
        | false, true -> -1
        | _, _ -> 0 )
      | _ as result -> result )
end

type t = SourceFile.t list Reference.Table.t

let insert_source_file ~inserted existing_files =
  let rec insert sofar = function
    | [] -> List.rev_append sofar [inserted]
    | current_file :: rest as existing -> (
      match SourceFile.same_module_compare inserted current_file with
      | 0 ->
          (* We have the following precondition for files that are in the same module: *)
          (* `same_module_compare a b = 0` implies `equal a b` *)
          assert (SourceFile.equal inserted current_file);

          (* Duplicate entry detected. Do nothing *)
          existing_files
      | x when x > 0 -> List.rev_append sofar (inserted :: existing)
      | _ -> insert (current_file :: sofar) rest )
  in
  insert [] existing_files


let remove_source_file ~removed existing_files =
  let rec remove sofar = function
    | [] -> existing_files
    | current_file :: rest -> (
      match SourceFile.same_module_compare removed current_file with
      | 0 ->
          (* We have the following precondition for files that are in the same module: *)
          (* `same_module_compare a b = 0` implies `equal a b` *)
          assert (SourceFile.equal removed current_file);
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


let create configuration =
  let tracker = Reference.Table.create () in
  let process_file path =
    match SourceFile.create ~configuration path with
    | None -> ()
    | Some source_file ->
        let qualifier = SourceFile.qualifier source_file in
        let update_table = function
          | None -> [source_file]
          | Some source_files -> insert_source_file ~inserted:source_file source_files
        in
        Hashtbl.update tracker qualifier ~f:update_table
  in
  let files = find_files configuration in
  List.iter files ~f:process_file;
  tracker


let lookup tracker module_name =
  match Hashtbl.find tracker module_name with
  | Some (source_file :: _) -> Some source_file
  | _ -> None


let source_files tracker = Hashtbl.data tracker |> List.filter_map ~f:List.hd

module FileSystemEvent = struct
  type t =
    | Update of Path.t
    | Remove of Path.t

  let create path = if Path.file_exists path then Update path else Remove path
end

module IncrementalUpdate = struct
  type t =
    | New of SourceFile.t
    | Delete of Reference.t
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]
end

let update ~configuration ~paths tracker =
  (* Process a single filesystem event *)
  let process_filesystem_event ~configuration tracker = function
    | FileSystemEvent.Update path -> (
      match SourceFile.create ~configuration path with
      | None ->
          Log.warning "`%a` not found in search path." Path.pp path;
          None
      | Some source_file -> (
          let qualifier = SourceFile.qualifier source_file in
          match Hashtbl.find tracker qualifier with
          | None ->
              (* New file for a new module *)
              Hashtbl.set tracker ~key:qualifier ~data:[source_file];
              Some (IncrementalUpdate.New source_file)
          | Some source_files ->
              let new_source_files = insert_source_file ~inserted:source_file source_files in
              let new_source_file = List.hd_exn new_source_files in
              Hashtbl.set tracker ~key:qualifier ~data:new_source_files;
              if SourceFile.equal new_source_file source_file then
                (* Updating a shadowing file means the module gets changed *)
                Some (IncrementalUpdate.New source_file)
              else (* Updating a shadowed file should not trigger any reanalysis *)
                None ) )
    | FileSystemEvent.Remove path -> (
      match SourceFile.create ~configuration path with
      | None ->
          Log.warning "`%a` not found in search path." Path.pp path;
          None
      | Some source_file -> (
          let qualifier = SourceFile.qualifier source_file in
          Hashtbl.find tracker qualifier
          >>= fun source_files ->
          match source_files with
          | [] ->
              (* This should never happen but handle it just in case *)
              Hashtbl.remove tracker qualifier;
              None
          | old_source_file :: _ -> (
            match remove_source_file ~removed:source_file source_files with
            | [] ->
                (* Last remaining file for the module gets removed. *)
                Hashtbl.remove tracker qualifier;
                Some (IncrementalUpdate.Delete qualifier)
            | new_source_file :: _ as new_source_files ->
                Hashtbl.set tracker ~key:qualifier ~data:new_source_files;
                if SourceFile.equal old_source_file new_source_file then
                  (* Removing a shadowed file should not trigger any reanalysis *)
                  None
                else (* Removing source_file un-shadows another source file. *)
                  Some (IncrementalUpdate.New new_source_file) ) ) )
  in
  (* Make sure we have only one update per module *)
  let merge_updates updates =
    let table = Reference.Table.create () in
    let process_update update =
      match update with
      | IncrementalUpdate.New source_file ->
          let qualifier = SourceFile.qualifier source_file in
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
  |> List.filter_map ~f:(process_filesystem_event ~configuration tracker)
  |> merge_updates
