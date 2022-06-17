(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module T = struct
  type t = {
    relative: string;
    qualifier: Reference.t;
    priority: int;
    is_stub: bool;
    is_external: bool;
    is_init: bool;
  }
  [@@deriving compare, hash, sexp]
end

include Hashable.Make (T)
include T

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
          let strip_stub_suffix name =
            (* Stub packages have their directories named as `XXX-stubs`. See PEP 561. *)
            match String.chop_suffix name ~suffix:"-stubs" with
            | Some result -> result
            | None -> name
          in
          Filename.parts relative
          |> (* Strip current directory. *) List.tl_exn
          |> List.map ~f:strip_stub_suffix
          |> List.rev
        in
        let last_without_suffix =
          let last = List.hd_exn reversed_elements in
          match String.rindex last '.' with
          | Some index -> String.slice last 0 index
          | _ -> last
        in
        let strip = function
          | ["builtins"]
          | ["builtins"; "future"] ->
              []
          | "__init__" :: tail -> tail
          | elements -> elements
        in
        last_without_suffix :: List.tl_exn reversed_elements
        |> strip
        |> List.rev_map ~f:(String.split ~on:'.')
        |> List.concat
      in
      Reference.create_from_list qualifier


let create_from_search_path ~is_external ~search_paths ?extension path =
  SearchPath.search_for_path ~search_paths path
  >>= fun SearchPath.{ relative_path; priority } ->
  let relative = PyrePath.RelativePath.relative relative_path in
  let qualifier =
    match extension with
    | Some { Configuration.Extension.include_suffix_in_module_qualifier; _ }
      when include_suffix_in_module_qualifier ->
        (* Ensure extension is not stripped when creating qualifier *)
        qualifier_of_relative (relative ^ ".py")
    | _ -> qualifier_of_relative relative
  in
  let is_stub = PyrePath.is_path_python_stub relative in
  let is_init = PyrePath.is_path_python_init relative in
  Some { relative; qualifier; priority; is_stub; is_external; is_init }


let is_internal_path
    ~configuration:{ Configuration.Analysis.filter_directories; ignore_all_errors; _ }
    path
  =
  let original_raw_path =
    let raw_path = ArtifactPath.raw path in
    (* NOTE(grievejia): Symlink are generally not followed by the type checker. This usage comes
       from legacy code and should not be replicated elsewhere. *)
    PyrePath.follow_symbolic_link raw_path |> Option.value ~default:raw_path
  in
  let source_path_is_covered item =
    PyrePath.equal item original_raw_path
    || PyrePath.directory_contains ~directory:item original_raw_path
  in
  let filter_directories = Option.value filter_directories ~default:[] in
  let ignore_all_errors = Option.value ignore_all_errors ~default:[] in
  List.exists filter_directories ~f:source_path_is_covered
  && not (List.exists ignore_all_errors ~f:source_path_is_covered)


let should_type_check
    ~configuration:({ Configuration.Analysis.analyze_external_sources; _ } as configuration)
    path
  =
  analyze_external_sources || is_internal_path ~configuration path


let create ~configuration:({ Configuration.Analysis.excludes; _ } as configuration) path =
  let absolute_path = ArtifactPath.raw path |> PyrePath.absolute in
  let create ?extension path =
    let search_paths = Configuration.Analysis.search_paths configuration in
    let is_external = not (should_type_check ~configuration path) in
    create_from_search_path ~is_external ~search_paths ?extension path
  in
  let is_excluded =
    List.exists excludes ~f:(fun regexp -> Str.string_match regexp absolute_path 0)
  in
  let extension = Configuration.Analysis.find_extension configuration path in
  match is_excluded, extension with
  | true, _ -> None
  | _, Some extension -> create ~extension path
  | _, None when String.is_suffix ~suffix:".py" absolute_path -> create path
  | _, None when String.is_suffix ~suffix:".pyi" absolute_path -> create path
  | _ -> None


let qualifier { qualifier; _ } = qualifier

let is_in_project { is_external; _ } = not is_external

let create_for_testing ~relative ~is_external ~priority =
  let qualifier = qualifier_of_relative relative in
  let is_stub = PyrePath.is_path_python_stub relative in
  let is_init = PyrePath.is_path_python_init relative in
  { relative; qualifier; priority; is_stub; is_external; is_init }


let full_path ~configuration { relative; priority; _ } =
  let root =
    Configuration.Analysis.search_paths configuration
    |> fun search_paths -> List.nth_exn search_paths priority |> SearchPath.get_root
  in
  PyrePath.create_relative ~root ~relative |> ArtifactPath.create


(* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
   only. Do NOT use it on aribitrary SourceFiles. *)
let same_module_compare
    ~configuration
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
  let extensions = Configuration.Analysis.extension_suffixes configuration in
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
          | _, _ -> extension_priority ())
      | _ as result -> result)


let is_stub { is_stub; _ } = is_stub

let expand_relative_import ~from { is_init; qualifier; _ } =
  match Reference.show from with
  | "builtins" -> Reference.empty
  | serialized ->
      (* Expand relative imports according to PEP 328 *)
      let dots = String.take_while ~f:(fun dot -> Char.equal dot '.') serialized in
      let postfix =
        match String.drop_prefix serialized (String.length dots) with
        (* Special case for single `.`, `..`, etc. in from clause. *)
        | "" -> Reference.empty
        | nonempty -> Reference.create nonempty
      in
      let prefix =
        if not (String.is_empty dots) then
          let initializer_module_offset =
            (* `.` corresponds to the directory containing the module. For non-init modules, the
               qualifier matches the path, so we drop exactly the number of dots. However, for
               __init__ modules, the directory containing it represented by the qualifier. *)
            if is_init then
              1
            else
              0
          in
          List.rev (Reference.as_list qualifier)
          |> (fun reversed -> List.drop reversed (String.length dots - initializer_module_offset))
          |> List.rev
          |> Reference.create_from_list
        else
          Reference.empty
      in
      Reference.combine prefix postfix
