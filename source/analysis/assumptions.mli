(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Callable = Type.Callable

module ProtocolAssumptions : sig
  type t

  val find_assumed_protocol_parameters
    :  candidate:Type.t ->
    protocol:Identifier.t ->
    t ->
    Type.Parameter.t list option

  val add
    :  candidate:Type.t ->
    protocol:Identifier.t ->
    protocol_parameters:Type.Parameter.t list ->
    t ->
    t

  val empty : t
end

module CallableAssumptions : sig
  (* This should be removed when classes can be generic over TParams, and Callable can be treated
     like any other generic protocol. *)
  type t

  val find_assumed_callable_type : candidate:Type.t -> t -> Type.t option

  val add : candidate:Type.t -> callable:Type.t -> t -> t

  val empty : t
end

module DecoratorAssumptions : sig
  type t

  val add : t -> assume_is_not_a_decorator:Reference.t -> t

  val not_a_decorator : t -> candidate:Reference.t -> bool

  val empty : t
end

type t = {
  protocol_assumptions: ProtocolAssumptions.t;
  callable_assumptions: CallableAssumptions.t;
  decorator_assumptions: DecoratorAssumptions.t;
}
[@@deriving compare, sexp, hash, show]
