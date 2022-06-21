(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { configuration: Configuration.Analysis.t }

let create configuration = { configuration }

let configuration { configuration; _ } = configuration

let track_dependencies { configuration = { Configuration.Analysis.incremental_style; _ }; _ } =
  match incremental_style with
  | Configuration.Analysis.Shallow -> false
  | Configuration.Analysis.FineGrained -> true


let debug { configuration = { Configuration.Analysis.debug; _ }; _ } = debug

let python_version_info
    {
      configuration =
        {
          Configuration.Analysis.python_major_version;
          python_minor_version;
          python_micro_version;
          _;
        };
      _;
    }
  =
  python_major_version, python_minor_version, python_micro_version
