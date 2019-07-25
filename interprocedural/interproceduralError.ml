(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type kind = {
  code: int;
  name: string;
  messages: string list;
}
[@@deriving compare, eq, show, sexp, hash]

include Analysis.BaseError.Make (struct
  type t = kind

  let compare = compare_kind

  let hash = hash_kind

  let show = show_kind

  let hash_fold_t = hash_fold_kind

  let sexp_of_t = sexp_of_kind

  let t_of_sexp = kind_of_sexp

  let pp = pp_kind

  let equal = equal_kind

  let code { code; _ } = code

  let name { name; _ } = name

  let messages ~concise:_ ~signature:_ _ { messages; _ } = messages

  let inference_information ~signature:_ _ = `Null
end)
