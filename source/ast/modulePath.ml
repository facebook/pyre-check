(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre

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


module Raw = struct
  type t = {
    relative: string;
    priority: int;
  }
  [@@deriving compare, equal, hash, sexp]

  let empty = { relative = ""; priority = 0 }

  let pp formatter { relative; priority } = Format.fprintf formatter "%d/%s" priority relative

  (* NOTE: This comparator is expected to operate on SourceFiles that are mapped to the same module
     only. Do NOT use it on aribitrary SourceFiles. *)
  let priority_aware_compare
      ~configuration
      { relative = left_path; priority = left_priority }
      { relative = right_path; priority = right_priority }
    =
    let stub_comes_before_py_file _ =
      match PyrePath.is_path_python_stub left_path, PyrePath.is_path_python_stub right_path with
      | true, false -> -1
      | false, true -> 1
      | _, _ -> 0
    in
    let lower_path_priority_value_comes_first _ = Int.compare left_priority right_priority in
    let package_comes_before_file_module _ =
      match PyrePath.is_path_python_init left_path, PyrePath.is_path_python_init right_path with
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
end

module T = struct
  type t = {
    raw: Raw.t;
    qualifier: Reference.t;
    should_type_check: bool;
  }
  [@@deriving compare, hash, sexp]
end

include Hashable.Make (T)
include T

let equal = [%compare.equal: t]

let pp formatter { raw; qualifier; should_type_check } =
  let should_type_check = if not should_type_check then " [EXTERNAL]" else "" in
  Format.fprintf formatter "[%a(%a)%s]" Raw.pp raw Reference.pp qualifier should_type_check


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


let qualifier { qualifier; _ } = qualifier

let raw { raw; _ } = raw

let relative { raw = { Raw.relative; _ }; _ } = relative

let should_type_check { should_type_check; _ } = should_type_check

let create ~should_type_check ({ Raw.relative; _ } as raw) =
  let qualifier = qualifier_from_relative_path relative in
  { raw; qualifier; should_type_check }


let is_stub { raw = { Raw.relative; _ }; _ } = PyrePath.is_path_python_stub relative

let is_init { raw = { Raw.relative; _ }; _ } = PyrePath.is_path_python_init relative

let expand_relative_import ~from { qualifier; raw = { Raw.relative; _ }; _ } =
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
            if PyrePath.is_path_python_init relative then
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
