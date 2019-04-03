(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement


let apply ~define:({ Define.signature = { parameters; _ }; _ } as define) ~resolution:_ =
  if Define.has_decorator ~match_prefix:true define "$strip_first_parameter" then
    let parameters =
      List.tl parameters
      |> Option.value ~default:[]
    in
    let signature = { define.signature with parameters } in
    { define with signature }
  else
    define
