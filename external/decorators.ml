(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


let apply
    ~overload:{ Type.Callable.annotation; parameters }
    ~resolution:_
    ~name =
  match name with
  | "$strip_first_parameter" ->
      let parameters =
        match parameters with
        | Type.Callable.Defined parameters ->
            List.tl parameters
            |> Option.value ~default:parameters
            |> fun parameters -> Type.Callable.Defined parameters
        | Type.Callable.Undefined ->
            Type.Callable.Undefined
      in
      { Type.Callable.annotation; parameters }
  | _ ->
      { Type.Callable.annotation; parameters }
