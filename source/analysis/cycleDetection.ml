(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The CycleDetection.t data structure is a place where code involved in resolving attributes and
   constraint solving (all of which is mutually recursive logic in some cases) can attach notions of
   "visited" computations in order to break cycles that would lead to infinite recursion. The
   different cycle detection components are unrelated to one another other than all three of them
   break some kind of postential cycle.

   The `decorators_being_resolved` is used to record any time we attempt to resolve some name as a
   decorator; if in any later recursive call we encounter the same decorator we know to skip it and
   record an error saying the decorator cannot be resolved; this can be triggered by cyclic use of
   decorators (which would typically produce a runtime error); the simplest case is a decorator
   trying to decorate itself.

   The `assumed_callable_types` seems to get hit mainly when understanding the `__call__` method of
   a callable type as a `BoundMethod`. This particular cycle may just be an implementation detail of
   Pyre; the issue happens when we try to understand a `BoundMethod` as a callable; we end up
   resolving the `__call__` attribute through a special case that eventually calls back into an
   identical `resolve_callable_protocol` call, and there is probably perf to gain by handling it
   better. It is also possible to directly hit a cycle if people are annotating a `__call__`
   attribute directly as a (mutually) recursive type; ideally we would just reject this and ignore
   it which would rule out cycles.

   The `assumed_recursive_instantiations` is used to track any combination of a candidate `Type.t`
   and a recursive structural type - i.e. a protocol or a recursive alias - (as a bare name) that we
   might want to subtype; the process of subtyping involves solving for the type arguments on the
   protocol if it is generic, and we can hit recursive cases: for example a protocol with a method
   that returns itself. We break the cycle by basically assuming the candidate satisfies the
   protocol in any recursive call, then bubbling back out to the topmost call and checking for
   contradictions. This logic can be triggered both by generic protocols and by generic recursive
   type aliases (which share some type checking logic with protocols because both are structural
   types that are potentially recursive).

   All three of these cycle detectors are "simple cycle breaking" in the sense that the only place
   which *sets* some cycle detection is the same block of code that *uses* it; none of them are used
   to "magically" communicate between different parts of the code:

   - decorators_being_resolved is used when handling a decorator to resolve a define

   - assumed_callable_types is used when resolving a type "as a callable protocol"

   - assumed_recursive_instantiations is used when instantiating protocol / recursive alias
   params *)

open Core
open Ast
module Callable = Type.Callable

module DecoratorsBeingResolved = struct
  type t = Reference.t list [@@deriving compare, equal, sexp, hash, show]

  let add sofar ~assume_is_not_a_decorator = assume_is_not_a_decorator :: sofar

  let not_a_decorator sofar ~candidate = List.exists sofar ~f:(Reference.equal candidate)

  let empty = []
end

module AssumedCallableTypes = struct
  type callable_assumption = Type.t [@@deriving compare, equal, sexp, hash, show]

  type t = (Type.t * callable_assumption) list [@@deriving compare, equal, sexp, hash, show]

  let find_assumed_callable_type ~candidate cycle_detections =
    List.Assoc.find cycle_detections candidate ~equal:Type.equal


  let add ~candidate ~callable existing_cycle_detections =
    List.Assoc.add existing_cycle_detections candidate callable ~equal:Type.equal


  let empty = []
end

module AssumedRecursiveInstantiations = struct
  type target_parameters = Type.Argument.t list [@@deriving compare, equal, sexp, hash, show]

  type assumption = {
    candidate: Type.t;
    target: Identifier.t;
  }
  [@@deriving compare, sexp, hash, show, equal]

  type t = (assumption * target_parameters) list [@@deriving compare, equal, sexp, hash, show]

  let find_assumed_recursive_type_parameters ~candidate ~target cycle_detections =
    List.Assoc.find cycle_detections { candidate; target } ~equal:equal_assumption


  let add ~candidate ~target ~target_parameters existing_cycle_detections =
    List.Assoc.add
      existing_cycle_detections
      { candidate; target }
      target_parameters
      ~equal:equal_assumption


  let empty = []
end

type t = {
  decorators_being_resolved: DecoratorsBeingResolved.t;
  assumed_callable_types: AssumedCallableTypes.t;
  assumed_recursive_instantiations: AssumedRecursiveInstantiations.t;
}
[@@deriving compare, equal, sexp, hash, show]
