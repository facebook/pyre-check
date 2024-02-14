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
  [@@deriving sexp, compare]
end

type t = { get_source_db: unit -> Sourcedb.t }

val create_for_testing : ?get_source_db:(unit -> Sourcedb.t) -> unit -> t

val create_from_argument_json : Yojson.Safe.t -> (t, Error.t) Result.t

val create_from_argument_file : PyrePath.t -> (t, Error.t) result
