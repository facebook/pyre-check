(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression

let to_condition ~subject:_ ~case:_ =
  Expression.Constant Constant.True |> Node.create_with_default_location
