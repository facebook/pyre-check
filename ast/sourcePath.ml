(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Pyre

type t = {
  relative: string;
  qualifier: Reference.t;
  priority: int;
  is_stub: bool;
  is_external: bool;
  is_init: bool;
}
[@@deriving sexp, compare]

let equal = [%compare.equal: t]

let pp formatter { relative; qualifier; priority; is_stub; is_external; is_init } =
  let is_stub = if is_stub then " [STUB]" else "" in
  let is_external = if is_external then " [EXTERNAL]" else "" in
  let is_init = if is_init then " [INIT]" else "" in
  Format.fprintf
    formatter
    "[%d/%s(%a)%s%s%s]"
    priority
    relative
    Reference.pp
    qualifier
    is_stub
    is_external
    is_init


let qualifier_of_relative relative =
  match relative with
  | "" -> Reference.empty
  | _ ->
      let qualifier =
        let reversed_elements =
          Filename.parts relative |> (* Strip current directory. *) List.tl_exn |> List.rev
        in
        let last_without_suffix =
          let last = List.hd_exn reversed_elements in
          match String.rindex last '.' with
          | Some index -> String.slice last 0 index
          | _ -> last
        in
        let strip = function
          | "future" :: "builtins" :: tail
          | "builtins" :: tail ->
              tail
          | "__init__" :: tail -> tail
          | elements -> elements
        in
        last_without_suffix :: List.tl_exn reversed_elements
        |> strip
        |> List.rev_map ~f:(String.split ~on:'.')
        |> List.concat
      in
      Reference.create_from_list qualifier


let create_from_search_path ~is_external ~search_path path =
  SearchPath.search_for_path ~search_path path
  >>= fun SearchPath.{ relative_path; priority } ->
  let relative = Path.RelativePath.relative relative_path in
  let qualifier = qualifier_of_relative relative in
  let is_stub = Path.is_path_python_stub relative in
  let is_init = Path.is_path_python_init relative in
  Some { relative; qualifier; priority; is_stub; is_external; is_init }


let should_type_check
    ~configuration:{ Configuration.Analysis.filter_directories; ignore_all_errors; _ }
    path
  =
  let path = Path.follow_symbolic_link path |> Option.value ~default:path in
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


let full_path ~configuration { relative; priority; _ } =
  let root =
    Configuration.Analysis.search_path configuration
    |> fun search_paths -> List.nth_exn search_paths priority |> SearchPath.get_root
  in
  Path.create_relative ~root ~relative


(* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
   only. Do NOT use it on aribitrary SourceFiles. *)
let same_module_compare
    ~configuration:{ Configuration.Analysis.extensions; _ }
    {
      priority = left_priority;
      is_stub = left_is_stub;
      is_init = left_is_init;
      relative = left_path;
      _;
    }
    {
      priority = right_priority;
      is_stub = right_is_stub;
      is_init = right_is_init;
      relative = right_path;
      _;
    }
  =
  let extension_priority _ =
    (* If all else, equal, prioritize extensions in the order listed in the configuration. *)
    let find_extension_index path =
      let extensions =
        if Option.is_some (List.find extensions ~f:(String.equal ".py")) then
          extensions
        else
          ".py" :: extensions
      in
      let get_extension path =
        Filename.split_extension path
        |> snd
        >>| (fun extension -> "." ^ extension)
        |> Option.value ~default:""
      in
      List.findi extensions ~f:(fun _ extension -> String.equal (get_extension path) extension)
      >>| fst
    in
    match find_extension_index left_path, find_extension_index right_path with
    | Some left, Some right -> right - left
    | _ -> 0
  in
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
      | _, _ -> extension_priority () )
    | _ as result -> result )
