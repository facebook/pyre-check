(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module NamedTuples = NamedTuples
module Filter = Filter
module NewType = NewType

let apply_to_ast source = source |> NamedTuples.transform_ast |> NewType.transform_ast

let apply_to_environment environment resolution source =
  ClassDecorator.transform_dataclass environment resolution source;
  ClassDecorator.transform_attrs environment resolution source
