(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = private {
  relative: string;
  qualifier: Reference.t;
  priority: int;
  is_stub: bool;
  is_external: bool;
  is_init: bool;
}
[@@deriving compare, eq, hash, sexp]

include Hashable with type t := t

val pp : Format.formatter -> t -> unit

val create : configuration:Configuration.Analysis.t -> ArtifactPath.t -> t option

val qualifier : t -> Reference.t

val is_in_project : t -> bool

val create_for_testing : relative:string -> is_external:bool -> priority:int -> t

val qualifier_of_relative : string -> Reference.t

val full_path : configuration:Configuration.Analysis.t -> t -> ArtifactPath.t

(* Expose for testing *)
val same_module_compare : configuration:Configuration.Analysis.t -> t -> t -> int

val is_stub : t -> bool

val is_internal_path : configuration:Configuration.Analysis.t -> ArtifactPath.t -> bool

val expand_relative_import : from:Reference.t -> t -> Reference.t
