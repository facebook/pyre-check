(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

exception LoadError of string

val load_from_string : string -> (t, string) Result.t

val load : Configuration.UnwatchedFiles.t -> (t, string) Result.t

val load_exn : Configuration.UnwatchedFiles.t -> t

val empty : t

(* Expose for testing purpose *)
val of_alist_exn : (string * string) list -> t

(* Expose for testing purpose *)
val to_alist : t -> (string * string) list

module Difference : sig
  module Kind : sig
    type t =
      | New
      | Deleted
      | Changed
    [@@deriving sexp, compare]
  end

  type t = {
    kind: Kind.t;
    path: string;
  }
  [@@deriving sexp, compare]
end

val difference : original:t -> t -> Difference.t list
