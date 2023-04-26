(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre

module Raw = struct
  module T = struct
    type t = {
      relative: string;
      priority: int;
    }
    [@@deriving compare, hash, sexp]

    let pp formatter { relative; priority } = Format.fprintf formatter "%d/%s" priority relative

    let equal = [%compare.equal: t]

    let create ~configuration path =
      let search_paths = Configuration.Analysis.search_paths configuration in
      SearchPath.search_for_path ~search_paths path
      >>| fun SearchPath.{ relative_path; priority } ->
      { relative = PyrePath.RelativePath.relative relative_path; priority }


    let full_path ~configuration { relative; priority; _ } =
      let root =
        Configuration.Analysis.search_paths configuration
        |> fun search_paths -> List.nth_exn search_paths priority |> SearchPath.get_root
      in
      PyrePath.create_relative ~root ~relative |> ArtifactPath.create
  end

  include T
  module Set = Caml.Set.Make (T)
end

module T = struct
  type t = {
    raw: Raw.t;
    qualifier: Reference.t;
    is_stub: bool;
    is_external: bool;
    is_init: bool;
  }
  [@@deriving compare, hash, sexp]
end

include Hashable.Make (T)
include T

let equal = [%compare.equal: t]

let pp formatter { raw; qualifier; is_stub; is_external; is_init } =
  let is_stub = if is_stub then " [STUB]" else "" in
  let is_external = if is_external then " [EXTERNAL]" else "" in
  let is_init = if is_init then " [INIT]" else "" in
  Format.fprintf
    formatter
    "[%a(%a)%s%s%s]"
    Raw.pp
    raw
    Reference.pp
    qualifier
    is_stub
    is_external
    is_init


let file_name_parts_for_relative_path path =
  Filename.parts path
  |> (* `Filename.parts` for a relative path "foo/bar.py" returns `["."; "foo"; "bar.py"]`. Strip
        the current directory ".". *)
  List.tl_exn


let strip_stub_package path_parts =
  let strip_stub_suffix name =
    (* Stub packages have their directories named as `XXX-stubs`. See PEP 561. *)
    match String.chop_suffix name ~suffix:"-stubs" with
    | Some result -> result
    | None -> name
  in
  (* NOTE: We currently assume that `foo-stubs` may occur at any level in the path. This does not
     match PEP 561: https://peps.python.org/pep-0561/#stub-only-packages. *)
  List.map path_parts ~f:strip_stub_suffix


let is_in_stub_package path =
  let path_parts = file_name_parts_for_relative_path path in
  [%compare.equal: string list] (strip_stub_package path_parts) path_parts |> not


let qualifier_from_relative_path relative =
  match relative with
  | "" -> Reference.empty
  | _ ->
      let strip_implicit_qualifier_parts = function
        | ["builtins"]
        | ["builtins"; "future"] ->
            []
        | "__init__" :: tail -> tail
        | elements -> elements
      in
      let reversed_elements =
        file_name_parts_for_relative_path relative |> strip_stub_package |> List.rev
      in
      let last_without_suffix =
        let last = List.hd_exn reversed_elements in
        match String.rindex last '.' with
        | Some index -> String.slice last 0 index
        | _ -> last
      in
      last_without_suffix :: List.tl_exn reversed_elements
      |> strip_implicit_qualifier_parts
      |> List.rev_map ~f:(String.split ~on:'.')
      |> List.concat
      |> Reference.create_from_list


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
  if List.exists excludes ~f:(fun regexp -> Str.string_match regexp absolute_path 0) then
    None
  else
    Raw.create ~configuration path
    >>= fun (Raw.{ relative; _ } as raw) ->
    let qualifier =
      match Configuration.Analysis.find_extension configuration path with
      | Some { Configuration.Extension.include_suffix_in_module_qualifier; _ }
        when include_suffix_in_module_qualifier ->
          (* Ensure extension is not stripped when creating qualifier *)
          qualifier_from_relative_path (relative ^ ".py")
      | _ -> qualifier_from_relative_path relative
    in
    let is_stub = PyrePath.is_path_python_stub relative in
    let is_init = PyrePath.is_path_python_init relative in
    Some
      {
        raw;
        qualifier;
        is_stub;
        is_external = not (should_type_check ~configuration path);
        is_init;
      }


let qualifier { qualifier; _ } = qualifier

let raw { raw; _ } = raw

let relative { raw = { relative; _ }; _ } = relative

let is_in_project { is_external; _ } = not is_external

let create_for_testing ~relative ~is_external ~priority =
  let raw = Raw.{ relative; priority } in
  let qualifier = qualifier_from_relative_path relative in
  let is_stub = PyrePath.is_path_python_stub relative in
  let is_init = PyrePath.is_path_python_init relative in
  { raw; qualifier; is_stub; is_external; is_init }


let full_path ~configuration { raw; _ } = Raw.full_path ~configuration raw

(* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
   only. Do NOT use it on aribitrary SourceFiles. *)
let same_module_compare
    ~configuration
    {
      raw = { relative = left_path; priority = left_priority };
      is_stub = left_is_stub;
      is_init = left_is_init;
      _;
    }
    {
      raw = { relative = right_path; priority = right_priority };
      is_stub = right_is_stub;
      is_init = right_is_init;
      _;
    }
  =
  let stub_comes_before_py_file _ =
    match left_is_stub, right_is_stub with
    | true, false -> -1
    | false, true -> 1
    | _, _ -> 0
  in
  let lower_path_priority_value_comes_first _ = Int.compare left_priority right_priority in
  let package_comes_before_file_module _ =
    match left_is_init, right_is_init with
    | true, false -> -1
    | false, true -> 1
    | _, _ -> 0
  in
  let extension_order_from_configuration _ =
    let extensions = Configuration.Analysis.extension_suffixes configuration in
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
    | Some left, Some right -> left - right
    | _ -> 0
  in
  let vanilla_module_comes_before_dotted_file _ =
    let is_slash char = Char.equal char (Char.of_string "/") in
    String.count right_path ~f:is_slash - String.count left_path ~f:is_slash
  in
  let stub_package_shadows_regular_path _ =
    match is_in_stub_package left_path, is_in_stub_package right_path with
    | true, false -> -1
    | false, true -> 1
    | _ -> 0
  in
  let priority_order =
    [
      stub_comes_before_py_file;
      lower_path_priority_value_comes_first;
      package_comes_before_file_module;
      extension_order_from_configuration;
      stub_package_shadows_regular_path;
      vanilla_module_comes_before_dotted_file;
    ]
  in
  (* Return the first nonzero comparison *)
  List.find_map priority_order ~f:(fun priority_function ->
      match priority_function () with
      | 0 -> None
      | n -> Some n)
  |> Option.value ~default:0


let is_stub { is_stub; _ } = is_stub

let is_init { is_init; _ } = is_init

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


let equal_raw_paths { raw = left; _ } { raw = right; _ } = Raw.equal left right
