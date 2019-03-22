(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement


let apply ~define ~resolution:_ =
  if Define.has_decorator ~match_prefix:true define "$strip_first_parameter" then
    let parameters =
      List.tl parameters
      |> Option.value ~default:[]
    in
    { define with parameters }
  else
    define
