(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Pyre

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
