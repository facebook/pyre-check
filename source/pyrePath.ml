(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T170743593) new warning with ppx_conv_sexp.v0.16.X *)
[@@@warning "-name-out-of-scope"]

(* TODO(T132410158) Add a module-level doc comment. *)

(* Core shadows/deprecates the stdlib Unix module. *)
module CamlUnix = Unix
open Core

type path = string [@@deriving compare, show, sexp, hash]

type t =
  | Absolute of path
  | Relative of {
      root: path;
      relative: path;
    }
[@@deriving sexp, hash]

let absolute = function
  | Absolute path -> path
  | Relative { root; relative } -> root ^/ relative


let create_absolute ?(follow_symbolic_links = false) path =
  if follow_symbolic_links then
    Absolute (CamlUnix.realpath path)
  else
    Absolute path


let create_relative ~root ~relative = Relative { root = absolute root; relative }

let show = absolute

let equal left right = String.equal (absolute left) (absolute right)

let compare left right = String.compare (absolute left) (absolute right)

let pp format path = Format.fprintf format "%s" (absolute path)

let get_directory path = absolute path |> Filename.dirname |> create_absolute

let create_directory_recursively ?(permission = 0o777) path =
  let rec do_create = function
    | path when not (Stdlib.Sys.file_exists path) -> (
        match do_create (Filename.dirname path) with
        | Result.Error _ as error -> error
        | Result.Ok () ->
            CamlUnix.mkdir path permission;
            Result.Ok ())
    | path when Stdlib.Sys.is_directory path -> Result.Ok ()
    | path ->
        let message = Format.sprintf "A non-directory already exists: %s" path in
        Result.Error message
  in
  do_create (absolute path)


let ensure_parent_directory_exists ?permission path =
  create_directory_recursively ?permission (get_directory path)


let get_relative_to_root ~root ~path =
  let root =
    let root = absolute root in
    if not (String.is_suffix ~suffix:"/" root) then root ^ "/" else root
  in
  String.chop_prefix ~prefix:root (absolute path)


let current_working_directory () = create_absolute (Stdlib.Sys.getcwd ())

let append path ~element =
  match path with
  | Absolute path -> Absolute (path ^/ element)
  | Relative { root; relative } ->
      let relative =
        match relative with
        | "" -> element
        | _ -> relative ^/ element
      in
      Relative { root; relative }


let get_suffix_path = function
  | Absolute path -> path
  | Relative { relative; _ } -> relative


let is_path_python_stub path = String.is_suffix ~suffix:".pyi" path

let is_path_python_init path =
  String.is_suffix ~suffix:"__init__.pyi" path || String.is_suffix ~suffix:"__init__.py" path


let rec stat_no_eintr_raw path =
  try Some (CamlUnix.stat path) with
  | CamlUnix.Unix_error (CamlUnix.EINTR, _, _) -> stat_no_eintr_raw path
  | CamlUnix.Unix_error ((CamlUnix.ENOENT | CamlUnix.ENOTDIR), _, _) -> None


let stat_no_eintr path = stat_no_eintr_raw (absolute path)

let file_exists path = Option.is_some (stat_no_eintr path)

let directory_exists path =
  match stat_no_eintr path with
  | Some { CamlUnix.st_kind = CamlUnix.S_DIR; _ } -> true
  | _ -> false


let last path =
  let absolute = absolute path in
  String.split ~on:'/' absolute |> List.last |> Option.value ~default:absolute


let follow_symbolic_link path =
  try absolute path |> create_absolute ~follow_symbolic_links:true |> Option.some with
  | CamlUnix.Unix_error _ -> None


(* Variant of Sys.readdir where names are sorted in alphabetical order *)
let read_directory_ordered_raw path =
  let entries = Stdlib.Sys.readdir path in
  Array.sort ~compare:String.compare entries;
  entries


let list ?(file_filter = fun _ -> true) ?(directory_filter = fun _ -> true) ~root () =
  let rec list sofar path =
    match stat_no_eintr_raw path with
    | Some { CamlUnix.st_kind = CamlUnix.S_DIR; _ } ->
        if directory_filter path then (
          match read_directory_ordered_raw path with
          | entries ->
              let collect sofar entry = list sofar (path ^/ entry) in
              Array.fold ~init:sofar ~f:collect entries
          | exception Sys_error _ ->
              Log.error "Could not list `%s`" path;
              sofar)
        else
          sofar
    | _ when file_filter path -> create_absolute path :: sofar
    | _ -> sofar
  in
  list [] (absolute root)


let read_directory_ordered path =
  absolute path
  |> read_directory_ordered_raw
  |> Array.map ~f:(fun relative -> create_relative ~root:path ~relative)
  |> Array.to_list


let directory_contains ~directory path =
  let path = absolute path in
  let directory = absolute directory in
  String.is_prefix ~prefix:(directory ^ "/") path


module FileType = struct
  type t =
    | File
    | Directory
end

(* Walk up from the root to try and find a directory/target. *)
let search_upwards ~target ~target_type ~root =
  let rec directory_has_target directory =
    match target_type, stat_no_eintr_raw (directory ^/ target) with
    | FileType.Directory, Some { CamlUnix.st_kind = CamlUnix.S_DIR; _ }
    | FileType.File, Some { CamlUnix.st_kind = CamlUnix.S_REG; _ } ->
        Some (create_absolute directory)
    | _ ->
        let parent_directory = Filename.dirname directory in
        if [%compare.equal: path] parent_directory directory then
          None
        else
          directory_has_target parent_directory
  in
  directory_has_target (absolute root)


let unlink_if_exists path =
  try CamlUnix.unlink (absolute path) with
  | CamlUnix.Unix_error ((CamlUnix.ENOENT | CamlUnix.ENOTDIR), _, _) -> ()


let remove_recursively path =
  let rec do_remove path =
    try
      let stats = CamlUnix.lstat path in
      match stats.CamlUnix.st_kind with
      | CamlUnix.S_DIR ->
          let contents = Stdlib.Sys.readdir path in
          List.iter (Array.to_list contents) ~f:(fun name ->
              let name = Filename.concat path name in
              do_remove name);
          CamlUnix.rmdir path
      | CamlUnix.S_LNK
      | CamlUnix.S_REG
      | CamlUnix.S_CHR
      | CamlUnix.S_BLK
      | CamlUnix.S_FIFO
      | CamlUnix.S_SOCK ->
          CamlUnix.unlink path
    with
    (* Path has been deleted out from under us - can ignore it. *)
    | Sys_error message ->
        Log.warning "Error occurred when removing %s recursively: %s" path message;
        ()
    | CamlUnix.Unix_error (CamlUnix.ENOENT, _, _) -> ()
  in
  do_remove (absolute path)


let remove_contents_of_directory path =
  try
    Stdlib.Sys.readdir (absolute path)
    |> Array.iter ~f:(fun relative -> remove_recursively (create_relative ~root:path ~relative));
    Result.Ok ()
  with
  | Sys_error message -> Result.Error message


let with_suffix path ~suffix =
  match path with
  | Absolute prefix -> Absolute (prefix ^ suffix)
  | Relative { root; relative } -> Relative { root; relative = relative ^ suffix }


let get_matching_files_recursively ~suffix ~paths =
  let rec expand path =
    if directory_exists path then
      let expand_directory_entry entry =
        let path = append path ~element:entry in
        if directory_exists path then
          expand path
        else if String.is_suffix ~suffix entry then
          [path]
        else
          []
      in
      Stdlib.Sys.readdir (absolute path)
      |> Array.to_list
      |> List.concat_map ~f:expand_directory_entry
    else if String.is_suffix ~suffix (absolute path) then
      [path]
    else
      []
  in
  List.concat_map ~f:expand paths
