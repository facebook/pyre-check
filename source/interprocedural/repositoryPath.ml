(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Represents the path to a file in the repository. *)

type t = {
  (* Relative path to the root of the repository.
   * None if the file is outside the repository. *)
  filename: string option;
  (* Absolute path. Used for debugging. *)
  path: PyrePath.t;
}
[@@deriving show]
