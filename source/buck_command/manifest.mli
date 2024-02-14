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
  [@@deriving sexp, compare]
end

module Item : sig
  type t = {
    artifact_path: string;
    source_path: string;
  }
  [@@deriving sexp, compare]
end

type t = Item.t list [@@deriving sexp, compare]

val load_from_string : string -> (t, Error.t) result

val load_from_file : PyrePath.t -> (t, Error.t) result

val to_alist : t -> (string * string) list
