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


  let create ~configuration:{ Configuration.Analysis.local_root; search_path; excludes; _ } path =
    let absolute_path = Path.absolute path in
    match List.exists excludes ~f:(fun regexp -> Str.string_match regexp absolute_path 0) with
    | true -> None
    | false -> (
      match create_from_search_path ~is_external:true ~search_path path with
      | Some _ as result -> result
      | None ->
          let search_path = [SearchPath.Root local_root] in
          create_from_search_path ~is_external:false ~search_path path )


  (* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
     only. Do NOT use it on aribitrary SourceFiles. *)
  let same_module_compare
      { priority = left_priority;
        is_stub = left_is_stub;
        is_external = left_is_external;
        is_init = left_is_init
      ; _
      }
      { priority = right_priority;
        is_stub = right_is_stub;
        is_external = right_is_external;
        is_init = right_is_init
      ; _
      }
    =
    (* Stub file always takes precedence *)
    match left_is_stub, right_is_stub with
    | true, false -> 1
    | false, true -> -1
    | _, _ -> (
      (* External source takes precedence over user code *)
      match left_is_external, right_is_external with
      | true, false -> 1
      | false, true -> -1
      | _, _ -> (
        (* Package takes precedence over file module with the same name *)
        match left_is_init, right_is_init with
        | true, false -> 1
        | false, true -> -1
        | _, _ ->
            (* Smaller int means higher priority *)
            Int.compare right_priority left_priority ) )
end

type t = SourceFile.t list Reference.Table.t

let insert_source_file new_file existing_files =
  let rec insert sofar = function
    | [] -> List.rev_append sofar [new_file]
    | current_file :: rest as existing -> (
      match SourceFile.same_module_compare new_file current_file with
      | 0 ->
          (* We have the following precondition for files that are in the same module: *)
          (* `same_module_compare a b = 0` implies `equal a b` *)
          assert (SourceFile.equal new_file current_file);

          (* Duplicate entry detected. Do nothing *)
          existing_files
      | x when x > 0 -> List.rev_append sofar (new_file :: existing)
      | _ -> insert (current_file :: sofar) rest )
  in
  insert [] existing_files


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
          | Some source_files -> insert_source_file source_file source_files
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
