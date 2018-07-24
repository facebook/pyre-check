(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module NamedTuples = PluginNamedTuples
module NewType = PluginNewType
module DataClass = PluginDataClass
module Filter = PluginFilter


let apply_to_ast source =
  source
  |> NamedTuples.transform_ast
  |> NewType.transform_ast
  |> DataClass.transform_ast
