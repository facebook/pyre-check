(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module Module = struct
  (** A helper type that help specifying a Python module. *)
  type t =
    | OfPath of string
        (** Specify a module at the given file path. The path is expected to be absolute. Symlinks
            will not be followed.*)
    | OfName of string  (** Specify a module with a given dot-qualified name directly. *)
  [@@deriving sexp, compare, yojson { strict = false }]
end

type t =
  | Stop
  | GetTypeErrors of {
      module_: Module.t; [@key "module"]
      overlay_id: string option;
    }
  | LocalUpdate of {
      module_: Module.t; [@key "module"]
      content: string;
      overlay_id: string;
    }
[@@deriving sexp, compare, yojson { strict = false }]

let of_string message =
  try
    Yojson.Safe.from_string message
    |> of_yojson
    |> Result.map_error ~f:(fun _ -> "Unrecognized request JSON")
  with
  | Yojson.Json_error message ->
      let message = Format.sprintf "Cannot parse JSON. %s" message in
      Result.Error message
