(** Copyright 2016-present Facebook. All rights reserved. **)

open Core


type path = string
[@@deriving eq, show, sexp]


type absolute = path
[@@deriving eq, show, sexp]


type relative = {
  root: path;
  relative: path;
}
[@@deriving eq, show, sexp]


type t =
  | Absolute of absolute
  | Relative of relative
[@@deriving sexp]


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


let pp format path =
  Format.fprintf format "%s" (absolute path)


let create_absolute path =
  Absolute (Filename.realpath path)


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


let follow_symlinks path =
  let rec follow_symlinks absolute =
    let is_symlink =
      Sys.file_exists absolute = `Yes &&
      let { Unix.st_kind; _ } = Unix.lstat absolute in
      st_kind = Unix.S_LNK
    in
    if is_symlink then
      follow_symlinks (Unix.readlink absolute)
    else
      absolute
  in
  Absolute (follow_symlinks (absolute path))


let directory_contains ~directory ~path =
  let directory = absolute directory in
  let path = absolute path in
  String.is_prefix ~prefix:directory path


let remove path =
  try
    Sys.remove (absolute path)
  with Sys_error _ ->
    Log.debug "Unable to remove file at %a" pp path


module Map = Map.Make(struct
    type nonrec t = t
    let compare left right = String.compare (absolute left) (absolute right)
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)
