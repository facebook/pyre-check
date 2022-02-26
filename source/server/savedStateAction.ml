(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t =
  | LoadFromFile of {
      shared_memory_path: PyrePath.t;
      changed_files_path: PyrePath.t option;
    }
  | LoadFromProject of {
      project_name: string;
      project_metadata: string option;
    }
  | SaveToFile of { shared_memory_path: PyrePath.t }
[@@deriving sexp, compare, hash]

let of_yojson json =
  let open Yojson.Safe.Util in
  let parsing_failed () =
    let message =
      Format.sprintf "Malformed saved state action JSON: %s" (Yojson.Safe.to_string json)
    in
    Result.Error message
  in
  match json with
  | `List [`String "load_from_file"; load_from_file_options] -> (
      match
        ( member "shared_memory_path" load_from_file_options,
          member "changed_files_path" load_from_file_options )
      with
      | `String shared_memory_path, `String changed_files_path ->
          Result.Ok
            (LoadFromFile
               {
                 shared_memory_path = PyrePath.create_absolute shared_memory_path;
                 changed_files_path = Some (PyrePath.create_absolute changed_files_path);
               })
      | `String shared_memory_path, `Null ->
          Result.Ok
            (LoadFromFile
               {
                 shared_memory_path = PyrePath.create_absolute shared_memory_path;
                 changed_files_path = None;
               })
      | _, _ -> parsing_failed ())
  | `List [`String "load_from_project"; load_from_project_options] -> (
      match
        ( member "project_name" load_from_project_options,
          member "project_metadata" load_from_project_options )
      with
      | `String project_name, `String project_metadata ->
          Result.Ok (LoadFromProject { project_name; project_metadata = Some project_metadata })
      | `String project_name, `Null ->
          Result.Ok (LoadFromProject { project_name; project_metadata = None })
      | _, _ -> parsing_failed ())
  | `List [`String "save_to_file"; save_to_file_options] -> (
      match member "shared_memory_path" save_to_file_options with
      | `String shared_memory_path ->
          Result.Ok
            (SaveToFile { shared_memory_path = PyrePath.create_absolute shared_memory_path })
      | _ -> parsing_failed ())
  | _ -> parsing_failed ()


let to_yojson = function
  | LoadFromFile { shared_memory_path; changed_files_path } ->
      let load_from_file_options =
        let shared_memory_path_option =
          "shared_memory_path", `String (PyrePath.absolute shared_memory_path)
        in
        match changed_files_path with
        | None -> [shared_memory_path_option]
        | Some changed_files_path ->
            let changed_files_path_option =
              "changed_files_path", `String (PyrePath.absolute changed_files_path)
            in
            [shared_memory_path_option; changed_files_path_option]
      in
      `List [`String "load_from_file"; `Assoc load_from_file_options]
  | LoadFromProject { project_name; project_metadata } ->
      let load_from_project_options =
        let project_name_option = "project_name", `String project_name in
        match project_metadata with
        | None -> [project_name_option]
        | Some project_metadata ->
            let project_metadata_option = "project_metadata", `String project_metadata in
            [project_name_option; project_metadata_option]
      in
      `List [`String "load_from_project"; `Assoc load_from_project_options]
  | SaveToFile { shared_memory_path } ->
      let save_to_file_options =
        ["shared_memory_path", `String (PyrePath.absolute shared_memory_path)]
      in
      `List [`String "save_to_file"; `Assoc save_to_file_options]
