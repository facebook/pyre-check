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
}

let create_for_testing
    ?(get_source_db = fun () -> Sourcedb.create_for_testing ())
    ?(get_python_version = fun () -> Configuration.PythonVersion.default)
    ()
  =
  { get_source_db; get_python_version }


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
  }
  [@@deriving of_yojson { strict = false }]
end

let parse_py_version py_version =
  let do_parse ~major ?minor ?micro () =
    try
      let major = Int.of_string major in
      let minor = Option.map minor ~f:Int.of_string in
      let micro = Option.map micro ~f:Int.of_string in
      Result.Ok (Configuration.PythonVersion.create ~major ?minor ?micro ())
    with
    | Failure message -> Result.Error (Error.VersionFormatError { py_version; message })
  in
  match String.split py_version ~on:'.' with
  | [major] -> do_parse ~major ()
  | [major; minor] -> do_parse ~major ~minor ()
  | [major; minor; micro] -> do_parse ~major ~minor ~micro ()
  | _ ->
      Result.Error
        (Error.VersionFormatError { py_version; message = "Not of the form X or X.Y or X.Y.Z" })


let load_manifests filenames =
  let rec load sofar = function
    | [] -> Result.Ok (List.rev sofar)
    | current :: rest -> (
        match PyrePath.create_absolute current |> Manifest.load_from_file with
        | Result.Error error -> Result.Error (Error.ManifestError error)
        | Result.Ok manifest -> load (manifest :: sofar) rest)
  in
  load [] filenames


let create_from_arguments { Arguments.sources; dependencies; typeshed; py_version } =
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
