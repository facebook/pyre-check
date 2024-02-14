(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = { get_source_db: unit -> Sourcedb.t }

let create_for_testing ?(get_source_db = fun () -> Sourcedb.create_for_testing ()) () =
  { get_source_db }


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
  [@@deriving sexp, compare]
end

module Arguments = struct
  type t = {
    sources: string list; [@default []]
    dependencies: string list; [@default []]
    typeshed: string option; [@default None]
  }
  [@@deriving of_yojson { strict = false }]
end

let load_manifests filenames =
  let rec load sofar = function
    | [] -> Result.Ok (List.rev sofar)
    | current :: rest -> (
        match PyrePath.create_absolute current |> Manifest.load_from_file with
        | Result.Error error -> Result.Error (Error.ManifestError error)
        | Result.Ok manifest -> load (manifest :: sofar) rest)
  in
  load [] filenames


let create_from_arguments { Arguments.sources; dependencies; typeshed } =
  let open Result.Monad_infix in
  load_manifests sources
  >>= fun source_manifests ->
  load_manifests dependencies
  >>= fun dependency_manifests ->
  load_manifests (Option.to_list typeshed)
  >>= fun typeshed_manifests ->
  Result.Ok
    {
      get_source_db =
        Sourcedb.create_from_manifests ~source_manifests ~dependency_manifests ~typeshed_manifests;
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
