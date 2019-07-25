(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type kind = {
  code: int;
  name: string;
  messages: string list;
}
[@@deriving compare, eq, show, sexp, hash]

include Analysis.BaseError.Error with type kind := kind
