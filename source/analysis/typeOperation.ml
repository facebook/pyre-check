(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
    let apply_callable
        ~signature_select
        input_annotation
        ~callable_and_self:{ callable; self_argument }
      =
      match
        signature_select
          ~arguments:
            [
              {
                AttributeResolution.Argument.expression = None;
                kind = Ast.Expression.Call.Argument.Positional;
                resolved = input_annotation;
              };
            ]
          ~callable
          ~self_argument
      with
      | SignatureSelectionTypes.Found { selected_return_annotation } ->
          Some selected_return_annotation
      | _ -> None


    let compose
        ~signature_select
        { callable = original_left_callable; self_argument = left_self_argument }
        right_callable_and_self
      =
      let replace_return_annotation new_annotation ~input =
        {
          input with
          Type.Callable.implementation =
            { input.Type.Callable.implementation with annotation = new_annotation };
        }
      in
      let namespace = Type.Variable.Namespace.create_fresh () in
      let compose left_callable_namespaced =
        let left_free_variables =
          Type.Variable.all_free_variables (Type.Callable left_callable_namespaced)
        in
        Type.Callable.map
          ~f:(Type.Variable.mark_all_variables_as_bound ~specific:left_free_variables)
          left_callable_namespaced
        >>| (fun { Type.Callable.implementation = { annotation; _ }; _ } -> annotation)
        >>= apply_callable ~signature_select ~callable_and_self:right_callable_and_self
        >>| Type.Variable.mark_all_variables_as_free
              ~specific:(List.map ~f:Type.Variable.mark_as_bound left_free_variables)
        >>| replace_return_annotation ~input:left_callable_namespaced
        >>| fun result -> { callable = result; self_argument = left_self_argument }
      in
      Type.Callable.map
        ~f:(Type.Variable.namespace_all_free_variables ~namespace)
        original_left_callable
      >>= compose


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
