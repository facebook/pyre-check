(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast

let base_is_from_placeholder_stub base_expression ~aliases ~is_from_empty_stub =
  let parsed = Expression.delocalize base_expression |> Type.create ~aliases in
  match parsed with
  | Type.Primitive primitive
  | Parametric { name = primitive; _ } ->
      Reference.create primitive |> fun reference -> is_from_empty_stub reference
  | _ -> false
