(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type path = string [@@deriving compare, show, sexp, hash]

module AbsolutePath = struct
  type t = path [@@deriving compare, show, sexp, hash]
end

module RelativePath = struct
  type t = {
    root: path;
    relative: path;
  }
  [@@deriving compare, show, sexp, hash]

  let relative { relative; _ } = relative
end

type t =
  | Absolute of AbsolutePath.t
  | Relative of RelativePath.t
[@@deriving sexp, hash]

let absolute = function
  | Absolute path -> path
  | Relative { root; relative } -> root ^/ relative


let create_absolute ?(follow_symbolic_links = false) path =
  if follow_symbolic_links then
    Absolute (Filename.realpath path)
  else
    Absolute path


let create_relative ~root ~relative =
  let root =
    let root = absolute root in
    if not (String.is_suffix ~suffix:"/" root) then root ^ "/" else root
  in
  let relative = String.chop_prefix ~prefix:root relative |> Option.value ~default:relative in
  Relative { root; relative }


let show = absolute

let to_yojson path = `String (show path)

let equal left right = String.equal (absolute left) (absolute right)

let compare left right = String.compare (absolute left) (absolute right)

let pp format path = Format.fprintf format "%s" (absolute path)

let get_directory path = absolute path |> Filename.dirname |> create_absolute

let create_directory_recursively ?permission path =
  let rec do_create = function
    | path when not (Caml.Sys.file_exists path) -> (
        match do_create (Filename.dirname path) with
        | Result.Error _ as error -> error
        | Result.Ok () ->
            Unix.mkdir path ?perm:permission;
            Result.Ok ())
    | path when Caml.Sys.is_directory path -> Result.Ok ()
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


let current_working_directory () = create_absolute (Sys.getcwd ())

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


let is_directory path =
  absolute path
  |> fun path ->
  match Sys.is_directory path with
  | `Yes -> true
  | `No
  | `Unknown ->
      false


let get_suffix_path = function
  | Absolute path -> path
  | Relative { relative; _ } -> relative


let is_path_python_stub path = String.is_suffix ~suffix:".pyi" path

let is_path_python_init path =
  String.is_suffix ~suffix:"__init__.pyi" path || String.is_suffix ~suffix:"__init__.py" path


let file_exists path =
  absolute path
  |> fun path ->
  match Sys.file_exists path with
  | `Yes -> true
  | `No
  | `Unknown ->
      false


let last path =
  let absolute = absolute path in
  String.split ~on:'/' absolute |> List.last |> Option.value ~default:absolute


let follow_symbolic_link path =
  try absolute path |> create_absolute ~follow_symbolic_links:true |> Option.some with
  | Unix.Unix_error _ -> None


(* Variant of Sys.readdir where names are sorted in alphabetical order *)
let read_directory_ordered path =
  let entries = Core.Sys.readdir path in
  Array.sort ~compare:String.compare entries;
  entries


let list ?(file_filter = fun _ -> true) ?(directory_filter = fun _ -> true) ~root () =
  let rec list sofar path =
    match Core.Sys.is_directory path with
    | `Yes ->
        if directory_filter path then (
          match read_directory_ordered path with
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
  let exists =
    match target_type with
    | FileType.File -> Sys.is_file
    | Directory -> Sys.is_directory
  in
  let rec directory_has_target directory =
    match exists (directory ^/ target) with
    | `Yes -> Some (create_absolute directory)
    | _ when [%compare.equal: path] (Filename.dirname directory) directory -> None
    | _ -> directory_has_target (Filename.dirname directory)
  in
  directory_has_target (absolute root)


let remove path =
  try Sys.remove (absolute path) with
  | Sys_error _ -> Log.debug "Unable to remove file at %a" pp path


let remove_if_exists path =
  let path = absolute path in
  match Sys.file_exists path with
  | `Yes -> Core.Unix.remove path
  | `No
  | `Unknown ->
      ()


let remove_recursively path =
  let rec do_remove path =
    try
      let stats = Unix.lstat path in
      match stats.Unix.st_kind with
      | Unix.S_DIR ->
          let contents = Caml.Sys.readdir path in
          List.iter (Array.to_list contents) ~f:(fun name ->
              let name = Filename.concat path name in
              do_remove name);
          Unix.rmdir path
      | Unix.S_LNK
      | Unix.S_REG
      | Unix.S_CHR
      | Unix.S_BLK
      | Unix.S_FIFO
      | Unix.S_SOCK ->
          Unix.unlink path
    with
    (* Path has been deleted out from under us - can ignore it. *)
    | Sys_error message ->
        Log.warning "Error occurred when removing %s recursively: %s" path message;
        ()
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  do_remove (absolute path)


let remove_contents_of_directory path =
  try
    Caml.Sys.readdir (absolute path)
    |> Array.iter ~f:(fun relative -> remove_recursively (create_relative ~root:path ~relative));
    Result.Ok ()
  with
  | Sys_error message -> Result.Error message


module Map = Map.Make (struct
  type nonrec t = t

  let compare left right = String.compare (absolute left) (absolute right)

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare left right = String.compare (absolute left) (absolute right)

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

let with_suffix path ~suffix =
  match path with
  | Absolute prefix -> Absolute (prefix ^ suffix)
  | Relative { root; relative } -> Relative { root; relative = relative ^ suffix }


let get_matching_files_recursively ~suffix ~paths =
  let rec expand path =
    if is_directory path then
      let expand_directory_entry entry =
        let path = append path ~element:entry in
        if is_directory path then
          expand path
        else if String.is_suffix ~suffix entry then
          [path]
        else
          []
      in
      Sys.readdir (absolute path) |> Array.to_list |> List.concat_map ~f:expand_directory_entry
    else if String.is_suffix ~suffix (absolute path) then
      [path]
    else
      []
  in
  List.concat_map ~f:expand paths
