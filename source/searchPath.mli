(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Root of PyrePath.t
  | Subdirectory of {
      root: PyrePath.t;
      subdirectory: string;
    }
  | Submodule of {
      root: PyrePath.t;
      submodule: string;
    }
[@@deriving sexp, compare, hash, show, equal]

type search_result = {
  relative_path: string;  (** The relative path to one of the search root *)
  priority: int;  (** Smaller int means higher priority *)
}

val get_root : t -> PyrePath.t

val to_path : t -> PyrePath.t

(* Create search path from its string representation. This operation does NOT have filesystem
   side-effect. *)
val create : string -> t

val search_for_path : search_paths:t list -> ArtifactPath.t -> search_result option
