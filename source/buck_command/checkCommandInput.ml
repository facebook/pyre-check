(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  get_source_db: unit -> Sourcedb.t;
  get_python_version: unit -> Configuration.PythonVersion.t;
  get_system_platform: unit -> string option;
}

let create_for_testing
    ?(get_source_db = fun () -> Sourcedb.create ())
    ?(get_python_version = fun () -> Configuration.PythonVersion.create ())
    ?(get_system_platform = fun () -> None)
    ()
  =
  { get_source_db; get_python_version; get_system_platform }


module Error = struct
  type t =
    | FileReadError of {
        path: PyrePath.t;
        message: string;
      }
    | JsonParseError of {
        path: PyrePath.t option;
        message: string;
      }
    | JsonFormatError of {
        path: PyrePath.t option;
        message: string;
      }
    | ManifestError of Manifest.Error.t
    | VersionFormatError of {
        py_version: string;
        message: string;
      }
  [@@deriving sexp, compare]
end

module Arguments = struct
  type t = {
    sources: string list; [@default []]
    dependencies: string list; [@default []]
    typeshed: string option; [@default None]
    py_version: string;
    system_platform: string option; [@default None]
  }
  [@@deriving of_yojson { strict = false }]
end

let py_version_regex = Str.regexp {|\([0-9]+\)\(\.\([0-9]+\)\)?\(\.\([0-9]+\)\)?|}

let parse_py_version py_version =
  try
    let _ = Str.search_forward py_version_regex py_version 0 in
    let extract_number_from_index index =
      try Str.matched_group index py_version |> Int.of_string with
      | Stdlib.Not_found -> 0
    in
    let major = extract_number_from_index 1 in
    let minor = extract_number_from_index 3 in
    let micro = extract_number_from_index 5 in
    Result.Ok (Configuration.PythonVersion.create ~major ~minor ~micro ())
  with
  | Stdlib.Not_found ->
      Result.Error
        (Error.VersionFormatError { py_version; message = "Not of containing X or X.Y or X.Y.Z" })


let load_manifests filenames =
  let rec load sofar = function
    | [] -> Result.Ok (List.rev sofar)
    | current :: rest -> (
        match PyrePath.create_absolute current |> Manifest.load_from_file with
        | Result.Error error -> Result.Error (Error.ManifestError error)
        | Result.Ok manifest -> load (manifest :: sofar) rest)
  in
  load [] filenames


let create_from_arguments { Arguments.sources; dependencies; typeshed; py_version; system_platform }
  =
  let open Result.Monad_infix in
  load_manifests sources
  >>= fun source_manifests ->
  load_manifests dependencies
  >>= fun dependency_manifests ->
  load_manifests (Option.to_list typeshed)
  >>= fun typeshed_manifests ->
  parse_py_version py_version
  >>= fun parsed_python_version ->
  Result.Ok
    {
      get_source_db =
        Sourcedb.create_from_manifests ~source_manifests ~dependency_manifests ~typeshed_manifests;
      get_python_version = (fun () -> parsed_python_version);
      get_system_platform = (fun () -> system_platform);
    }


let create_from_argument_json json =
  match Arguments.of_yojson json with
  | Result.Error message -> Result.Error (Error.JsonFormatError { path = None; message })
  | Result.Ok arguments -> create_from_arguments arguments


let create_from_argument_file path =
  match Yojson.Safe.from_file (PyrePath.absolute path) |> create_from_argument_json with
  | _ as result -> result
  | exception Yojson.Json_error message ->
      Result.Error (Error.JsonParseError { path = Some path; message })
  | exception Sys_error message -> Result.Error (Error.FileReadError { path; message })
