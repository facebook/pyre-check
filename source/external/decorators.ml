(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let apply ~argument ~name =
  match name, argument with
  | "_strip_first_parameter_", Type.Callable callable ->
      let { Type.Callable.implementation = old_implementation; overloads = old_overloads; _ } =
        callable
      in
      let process_overload { Type.Callable.annotation; parameters } =
        let parameters =
          match parameters with
          | Type.Callable.Defined parameters ->
              List.tl parameters
              |> Option.value ~default:parameters
              |> fun parameters -> Type.Callable.Defined parameters
          | _ -> parameters
        in
        { Type.Callable.annotation; parameters }
      in
      Type.Callable
        {
          callable with
          implementation = process_overload old_implementation;
          overloads = List.map old_overloads ~f:process_overload;
        }
  | _, argument -> argument


let special_decorators = String.Set.singleton "_strip_first_parameter_"
