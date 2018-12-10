(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type path = string
[@@deriving eq, show, sexp, hash]


type absolute = path
[@@deriving eq, show, sexp, hash]


type relative = {
  root: path;
  relative: path;
}
[@@deriving eq, show, sexp, hash]


type t =
  | Absolute of absolute
  | Relative of relative
[@@deriving sexp, hash]


type path_t = t


let absolute = function
  | Absolute path -> path
  | Relative { root; relative } -> root ^/ relative


let relative = function
  | Absolute _ -> None
  | Relative { relative; _  } -> Some relative


let uri path =
  "file://" ^ (absolute path)


let show =
  absolute


let equal left right =
  String.equal (absolute left) (absolute right)


let compare left right =
  String.compare (absolute left) (absolute right)


let pp format path =
  Format.fprintf format "%s" (absolute path)


let create_absolute ?(follow_symbolic_links = true) path =
  if follow_symbolic_links then
    Absolute (Filename.realpath path)
  else
    Absolute path


let create_relative ~root ~relative =
  let root =
    let root = absolute root in
    if not (String.is_suffix ~suffix:"/" root) then root ^ "/" else root in
  let relative =
    String.chop_prefix ~prefix:root relative
    |> Option.value ~default:relative
  in
  Relative { root; relative }


let get_relative_to_root ~root ~path =
  let root =
    let root = absolute root in
    if not (String.is_suffix ~suffix:"/" root) then root ^ "/" else root
  in
  String.chop_prefix ~prefix:root (absolute path)


let from_uri uri =
  String.chop_prefix ~prefix:"file://" uri
  |> Option.map ~f:create_absolute


let current_working_directory () =
  create_absolute (Sys.getcwd ())


let append path ~element =
  match path with
  | Absolute path -> Absolute (path ^/ element)
  | Relative { root; relative } -> Relative { root; relative = relative ^/ element }


module AppendOperator = struct
  let (^|) path element =
    append path ~element
end


let is_directory path =
  absolute path
  |> fun path -> Sys.is_directory path = `Yes


let file_exists path =
  absolute path
  |> fun path -> Sys.file_exists path = `Yes


let last path =
  let absolute = absolute path in
  String.split ~on:'/' absolute
  |> List.last
  |> Option.value ~default:absolute


let real_path path =
  match path with
  | Absolute _ -> path
  | Relative _ -> absolute path |> create_absolute


let list ?(file_filter = fun _ -> true) ?(directory_filter = fun _ -> true) ~root () =
  let rec list sofar path =
    if Core.Sys.is_directory path = `Yes then
      begin
        if directory_filter path then
          match Core.Sys.ls_dir path with
          | entries ->
              let collect sofar entry =
                list sofar (path ^/ entry) in
              List.fold ~init:sofar ~f:collect entries
          | exception Sys_error _ ->
              Log.error "Could not list `%s`" path;
              sofar
        else
          sofar
      end
    else if file_filter path then
      (create_relative ~root ~relative:path) :: sofar
    else
      sofar
  in
  list [] (absolute root)


let directory_contains ?(follow_symlinks = false) ~directory path =
  try
    let path =
      if follow_symlinks then
        absolute path
        |> Filename.realpath
      else
        absolute path
    in
    let directory = absolute directory in
    String.is_prefix ~prefix:directory path
  with
  | Unix.Unix_error (error, name, parameters) ->
      Log.log_unix_error (error, name, parameters);
      false


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
  try
    Sys.remove (absolute path)
  with Sys_error _ ->
    Log.debug "Unable to remove file at %a" pp path


let readlink path =
  try
    Unix.readlink (absolute path)
    |> Option.some
  with Unix.Unix_error _ ->
    None


module Map = Map.Make(struct
    type nonrec t = t
    let compare left right = String.compare (absolute left) (absolute right)
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)

module SearchPath = struct
  type t =
    | Root of path_t
    | Subdirectory of { root: path_t; subdirectory: string }


  let equal left right =
    match left, right with
    | Root left, Root right ->
        equal left right
    | Subdirectory { root = left; subdirectory = left_subdirectory },
      Subdirectory { root = right; subdirectory = right_subdirectory } ->
        equal left right &&
        String.equal left_subdirectory right_subdirectory
    | _ ->
        false


  let get_root path =
    match path with
    | Root root -> root
    | Subdirectory { root; _ } -> root


  let to_path path =
    match path with
    | Root root ->
        root
    | Subdirectory { root; subdirectory } ->
        create_relative ~root ~relative:subdirectory


  let pp formatter path =
    pp formatter (to_path path)


  let show path = Format.asprintf "%a" pp path


  let create serialized =
    match String.split serialized ~on:'$' with
    | [root] ->
        Root (create_absolute root)
    | [root; subdirectory] ->
        Subdirectory { root = create_absolute root; subdirectory  }
    | _ ->
        failwith (Format.asprintf "Unable to create search path from %s" serialized)
end


let search_for_path ~search_path ~path =
  let under_root ~path root =
    get_relative_to_root ~root ~path
    |> Option.map ~f:(fun relative -> create_relative ~root ~relative)
  in
  search_path
  |> List.map ~f:SearchPath.get_root
  |> List.find_map ~f:(under_root ~path)
