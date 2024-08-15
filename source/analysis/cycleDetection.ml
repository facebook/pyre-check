(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
module Callable = Type.Callable

module AssumedProtocolInstantiations = struct
  type protocol_parameters = Type.Parameter.t list [@@deriving compare, sexp, hash, show]

  type assumption = {
    candidate: Type.t;
    protocol: Identifier.t;
  }
  [@@deriving compare, sexp, hash, show, eq]

  type t = (assumption * protocol_parameters) list [@@deriving compare, sexp, hash, show]

  let find_assumed_protocol_parameters ~candidate ~protocol cycle_detections =
    List.Assoc.find cycle_detections { candidate; protocol } ~equal:equal_assumption


  let add ~candidate ~protocol ~protocol_parameters existing_cycle_detections =
    List.Assoc.add
      existing_cycle_detections
      { candidate; protocol }
      protocol_parameters
      ~equal:equal_assumption


  let empty = []
end

module AssumedCallableTypes = struct
  type callable_assumption = Type.t [@@deriving compare, sexp, hash, show]

  type t = (Type.t * callable_assumption) list [@@deriving compare, sexp, hash, show]

  let find_assumed_callable_type ~candidate cycle_detections =
    List.Assoc.find cycle_detections candidate ~equal:Type.equal


  let add ~candidate ~callable existing_cycle_detections =
    List.Assoc.add existing_cycle_detections candidate callable ~equal:Type.equal


  let empty = []
end

module DecoratorsBeingResolved = struct
  type t = Reference.t list [@@deriving compare, sexp, hash, show]

  let add sofar ~assume_is_not_a_decorator = assume_is_not_a_decorator :: sofar

  let not_a_decorator sofar ~candidate = List.exists sofar ~f:(Reference.equal candidate)

  let empty = []
end

type t = {
  assumed_protocol_instantiations: AssumedProtocolInstantiations.t;
  assumed_callable_types: AssumedCallableTypes.t;
  decorators_being_resolved: DecoratorsBeingResolved.t;
}
[@@deriving compare, sexp, hash, show]
