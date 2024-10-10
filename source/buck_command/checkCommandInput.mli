(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Error : sig
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

type t = {
  get_source_db: unit -> Sourcedb.t;
  get_python_version: unit -> Configuration.PythonVersion.t;
  get_system_platform: unit -> string option;
}

val parse_py_version : string -> (Configuration.PythonVersion.t, Error.t) result

val create_for_testing
  :  ?get_source_db:(unit -> Sourcedb.t) ->
  ?get_python_version:(unit -> Configuration.PythonVersion.t) ->
  ?get_system_platform:(unit -> string option) ->
  unit ->
  t

val create_from_argument_json : Yojson.Safe.t -> (t, Error.t) Result.t

val create_from_argument_file : PyrePath.t -> (t, Error.t) result
