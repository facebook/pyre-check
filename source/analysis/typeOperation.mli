(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type callable_and_self_argument = {
  callable: Type.Callable.t;
  self_argument: Type.t option;
}
[@@deriving show, eq]

module TypeOperation : sig
  module Compose : sig
    val compose_list
      :  signature_select:
           (arguments:AttributeResolution.Argument.t list ->
           callable:Type.Callable.t ->
           self_argument:Type.t option ->
           SignatureSelectionTypes.instantiated_return_annotation) ->
      callable_and_self_argument list ->
      callable_and_self_argument option
  end
end
