(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

type t = {
  environment: Analysis.OverlaidEnvironment.t;
  open_files: OpenFiles.t;
}
