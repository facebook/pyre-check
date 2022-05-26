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
[@@deriving sexp, compare, hash, show, eq]

type search_result = {
  relative_path: PyrePath.RelativePath.t;
      (** The searched path relative to one of the search root *)
  priority: int;  (** Smaller int means higher priority *)
}

val get_root : t -> PyrePath.t

val to_path : t -> PyrePath.t

(* Create search path from its string representation. This operation does NOT have filesystem
   side-effect. *)
val create : string -> t

(* Create a normalized search path from its string representation. Normalizing a path means to
   expand its relativized root and follow symlinks. This operation DOES have filesystem side-effect. *)
val create_normalized : string -> t

(* Turn a potentially un-normalized search path into a normalized one. This operation DOES have
   filesystem side-effect.*)
val normalize : t -> t

val search_for_path : search_paths:t list -> ArtifactPath.t -> search_result option
