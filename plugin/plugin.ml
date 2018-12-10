(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis
open Environment

module NamedTuples = NamedTuples
module Filter = Filter


let apply_to_ast source =
  source
  |> NamedTuples.transform_ast
  |> NewType.transform_ast


let apply_to_environment (module Handler: Handler) resolution source =
  DataClass.transform_environment (module Handler: Handler) resolution source
