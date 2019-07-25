(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type path = string [@@deriving compare, eq, show, sexp, hash]

module AbsolutePath = struct
  type t = path [@@deriving compare, eq, show, sexp, hash]
end

module RelativePath = struct
  type t = {
    root: path;
    relative: path;
  }
  [@@deriving compare, eq, show, sexp, hash]

  let relative { relative; _ } = relative
end

type t =
  | Absolute of AbsolutePath.t
  | Relative of RelativePath.t
[@@deriving sexp, hash]

let absolute = function
  | Absolute path -> path
  | Relative { root; relative } -> root ^/ relative


let relative = function
  | Absolute _ -> None
  | Relative { relative; _ } -> Some relative


let uri path = "file://" ^ absolute path

let show = absolute

let to_yojson path = `String (show path)

let equal left right = String.equal (absolute left) (absolute right)

let compare left right = String.compare (absolute left) (absolute right)

let pp format path = Format.fprintf format "%s" (absolute path)

let create_absolute ?(follow_symbolic_links = true) path =
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


let get_relative_to_root ~root ~path =
  let root =
    let root = absolute root in
    if not (String.is_suffix ~suffix:"/" root) then root ^ "/" else root
  in
  String.chop_prefix ~prefix:root (absolute path)


let from_uri uri = String.chop_prefix ~prefix:"file://" uri |> Option.map ~f:create_absolute

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


module AppendOperator = struct
  let ( ^| ) path element = append path ~element
end

let is_directory path = absolute path |> fun path -> Sys.is_directory path = `Yes

let get_suffix_path = function
  | Absolute path -> path
  | Relative { relative; _ } -> relative


let is_path_python_stub path = String.is_suffix ~suffix:".pyi" path

let is_path_python_init path =
  String.is_suffix ~suffix:"__init__.pyi" path || String.is_suffix ~suffix:"__init__.py" path


let is_python_stub path = get_suffix_path path |> is_path_python_stub

let is_python_init path = get_suffix_path path |> is_path_python_init

let file_exists path = absolute path |> fun path -> Sys.file_exists path = `Yes

let last path =
  let absolute = absolute path in
  String.split ~on:'/' absolute |> List.last |> Option.value ~default:absolute


let real_path path =
  match path with
  | Absolute _ -> path
  | Relative _ -> absolute path |> create_absolute


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
    if Core.Sys.is_directory path = `Yes then
      if directory_filter path then (
        match read_directory_ordered path with
        | entries ->
            let collect sofar entry = list sofar (path ^/ entry) in
            Array.fold ~init:sofar ~f:collect entries
        | exception Sys_error _ ->
            Log.error "Could not list `%s`" path;
            sofar )
      else
        sofar
    else if file_filter path then
      create_relative ~root ~relative:path :: sofar
    else
      sofar
  in
  list [] (absolute root)


let directory_contains ~directory path =
  let path = absolute path in
  let directory = absolute directory in
  String.is_prefix ~prefix:directory path


(* Walk up from the root to try and find a directory/target. *)
let search_upwards ~target ~root =
  let rec directory_has_target directory =
    if Sys.is_file (directory ^/ target) = `Yes then
      Some (create_absolute directory)
    else if Filename.dirname directory = directory then
      None
    else
      directory_has_target (Filename.dirname directory)
  in
  directory_has_target (absolute root)


let remove path =
  try Sys.remove (absolute path) with
  | Sys_error _ -> Log.debug "Unable to remove file at %a" pp path


let readlink path =
  try Unix.readlink (absolute path) |> Option.some with
  | Unix.Unix_error _ -> None


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

let build_symlink_map ~links =
  let add_symlink map path =
    try
      let key = real_path path in
      Map.set map ~key ~data:path
    with
    | Unix.Unix_error (error, name, parameters) ->
        Log.log_unix_error ~section:`Warning (error, name, parameters);
        map
  in
  List.fold links ~init:Map.empty ~f:add_symlink


let with_suffix path ~suffix =
  match path with
  | Absolute prefix -> Absolute (prefix ^ suffix)
  | Relative { root; relative } -> Relative { root; relative = relative ^ suffix }


let get_directory path =
  absolute path |> Filename.dirname |> create_absolute ~follow_symbolic_links:false
