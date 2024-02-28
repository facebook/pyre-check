(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The PathLookup module provides helpers for converting between `SourcePath.t` and qualifiers
   (`Reference.t`).

   This always requires converting between `ArtifactPath.t` and `SourcePath.t` (the two are
   different because we need to account for builds that might remap the paths; for example under
   buck the `ArtifactPath.t` is the result of a buck build and the `SourcePath.t` is the user-facing
   path in the original source tree).

   The conversion functions have two forms, one that uses dependency injection for converting and
   another that takes a `BuildSystem.t` as input. *)

let qualifiers_of_source_path_with_build_system ~build_system ~source_code_api path =
  Analysis.SourcePaths.qualifiers_of_source_path
    ~lookup_artifact:(BuildSystem.lookup_artifact build_system)
    ~source_code_api
    path


let absolute_source_path_of_qualifier_with_build_system ~build_system ~source_code_api qualifier =
  Analysis.SourcePaths.absolute_source_path_of_qualifier
    ~lookup_source:(BuildSystem.lookup_source build_system)
    ~source_code_api
    qualifier
