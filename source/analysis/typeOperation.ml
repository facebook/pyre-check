(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type callable_and_self_argument = {
  callable: Type.Callable.t;
  self_argument: Type.t option;
}
[@@deriving show, eq]

module TypeOperation = struct
  module Compose = struct
    (* Approach: `compose` finds the composed function by passing in the return type of the left
       callable to the right callable. *)
    (* Variables need to be bound before being passed into `signature_select`, to properly simulate
       a function call, or else it won't work. *)
    (* We need to namespace the callable before we bind, or else we might have a collision with
       existing bound variables of the same name. *)
    let compose
        ~signature_select
        {
          callable =
            { Type.Callable.implementation = original_left_implementation; _ } as
            original_left_callable;
          self_argument = left_self_argument;
        }
        { callable = right_callable; self_argument = right_self_argument }
      =
      let select_and_unbind
          { Type.Callable.implementation = { annotation = left_annotation; _ }; _ }
        =
        match
          signature_select
            ~arguments:
              [
                {
                  AttributeResolution.Argument.expression = None;
                  kind = Ast.Expression.Call.Argument.Positional;
                  resolved = left_annotation;
                };
              ]
            ~callable:right_callable
            ~self_argument:right_self_argument
        with
        | SignatureSelectionTypes.Found { selected_return_annotation } ->
            let new_annotation =
              Type.Variable.mark_all_variables_as_free selected_return_annotation
            in
            Some
              {
                callable =
                  {
                    original_left_callable with
                    implementation =
                      { original_left_implementation with annotation = new_annotation };
                  };
                self_argument = left_self_argument;
              }
        | NotFound _ -> None
      in
      Type.Callable.map ~f:Type.Variable.mark_all_variables_as_bound original_left_callable
      >>= select_and_unbind


    let compose_list ~signature_select = function
      | [] -> None
      | first :: rest ->
          List.fold
            ~init:(Some first)
            ~f:(fun left right ->
              left >>= fun inner_left -> compose ~signature_select inner_left right)
            rest
  end
end
