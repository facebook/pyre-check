(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement


let apply
    ~define:({ Define.signature = { parameters; _ }; _ } as define)
    ~resolution:_
    ~name =
  match name with
  | "$strip_first_parameter" ->
      let parameters =
        List.tl parameters
        |> Option.value ~default:parameters
      in
      let signature = { define.signature with parameters } in
      { define with signature }
  | _ ->
      define
