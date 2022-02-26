(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | LoadFromFile of {
      shared_memory_path: PyrePath.t;
      changed_files_path: PyrePath.t option;
    }
  | LoadFromProject of {
      project_name: string;
      project_metadata: string option;
    }
  | SaveToFile of { shared_memory_path: PyrePath.t }
[@@deriving sexp, compare, hash, yojson]
