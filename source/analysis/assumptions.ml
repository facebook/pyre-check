(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
module Callable = Type.Callable

module ProtocolAssumptions = struct
  type protocol_parameters = Type.Parameter.t list [@@deriving compare, sexp, hash, show]

  type assumption = {
    candidate: Type.t;
    protocol: Identifier.t;
  }
  [@@deriving compare, sexp, hash, show, eq]

  type t = (assumption * protocol_parameters) list [@@deriving compare, sexp, hash, show]

  let find_assumed_protocol_parameters ~candidate ~protocol assumptions =
    List.Assoc.find assumptions { candidate; protocol } ~equal:equal_assumption


  let add ~candidate ~protocol ~protocol_parameters existing_assumptions =
    List.Assoc.add
      existing_assumptions
      { candidate; protocol }
      protocol_parameters
      ~equal:equal_assumption


  let empty = []
end

module CallableAssumptions = struct
  type callable_assumption = Type.t [@@deriving compare, sexp, hash, show]

  type t = (Type.t * callable_assumption) list [@@deriving compare, sexp, hash, show]

  let find_assumed_callable_type ~candidate assumptions =
    List.Assoc.find assumptions candidate ~equal:Type.equal


  let add ~candidate ~callable existing_assumptions =
    List.Assoc.add existing_assumptions candidate callable ~equal:Type.equal


  let empty = []
end

module DecoratorAssumptions = struct
  type t = Reference.t list [@@deriving compare, sexp, hash, show]

  let add sofar ~assume_is_not_a_decorator = assume_is_not_a_decorator :: sofar

  let not_a_decorator sofar ~candidate = List.exists sofar ~f:(Reference.equal candidate)

  let empty = []
end

type t = {
  protocol_assumptions: ProtocolAssumptions.t;
  callable_assumptions: CallableAssumptions.t;
  decorator_assumptions: DecoratorAssumptions.t;
}
[@@deriving compare, sexp, hash, show]
