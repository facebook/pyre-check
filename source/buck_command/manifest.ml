(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

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
  [@@deriving sexp, compare]
end

module Item = struct
  type t = {
    artifact_path: string;
    source_path: string;
  }
  [@@deriving sexp, compare]

  let to_pair { artifact_path; source_path } = artifact_path, source_path

  module Raw = struct
    type t = string * string * string [@@deriving of_yojson]
  end

  let load_from_raw (artifact_path, source_path, _) = { artifact_path; source_path }
end

type t = Item.t list [@@deriving sexp, compare]

module Raw = struct
  type t = Item.Raw.t list [@@deriving of_yojson]
end

let load_from_raw = List.map ~f:Item.load_from_raw

let load_from_string text =
  match Yojson.Safe.from_string text |> Raw.of_yojson with
  | Result.Ok raw -> Result.Ok (load_from_raw raw)
  | Result.Error message -> Result.Error (Error.JsonFormatError { path = None; message })
  | exception Yojson.Json_error message ->
      Result.Error (Error.JsonParseError { path = None; message })


let load_from_file path =
  match Yojson.Safe.from_file (PyrePath.absolute path) |> Raw.of_yojson with
  | Result.Ok result -> Result.Ok (load_from_raw result)
  | Result.Error message -> Result.Error (Error.JsonFormatError { path = Some path; message })
  | exception Yojson.Json_error message ->
      Result.Error (Error.JsonParseError { path = Some path; message })
  | exception Sys_error message -> Result.Error (Error.FileReadError { path; message })


let to_alist = List.map ~f:Item.to_pair
