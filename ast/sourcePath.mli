(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Pyre

type t = private {
  relative_path: Path.RelativePath.t;
  priority: int;
  is_stub: bool;
  is_external: bool;
  is_init: bool
}
[@@deriving sexp, compare, eq]

val pp : Format.formatter -> t -> unit

val create : configuration:Configuration.Analysis.t -> Path.t -> t option

val qualifier : t -> Reference.t

(* Expose for testing *)
val same_module_compare : t -> t -> int
