(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Interprocedural
open Domains

module Forward : sig
  type t = { source_taint: ForwardState.t } [@@deriving show]

  val empty : t
end

module Backward : sig
  type t = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving show]

  val empty : t
end

module Sanitizers : sig
  type t = {
    global: Sanitize.t;
    parameters: Sanitize.t;
    roots: SanitizeRootMap.t;
  }
  [@@deriving show]

  val empty : t
end

module Mode : sig
  type t =
    | Obscure
    | SkipAnalysis (* Don't analyze at all *)
    | SkipDecoratorWhenInlining
    | SkipOverrides
  [@@deriving show, compare]
end

module ModeSet : sig
  type t [@@deriving show]

  val singleton : Mode.t -> t

  val empty : t

  val is_empty : t -> bool

  val add : Mode.t -> t -> t

  val remove : Mode.t -> t -> t

  val contains : Mode.t -> t -> bool

  val join : t -> t -> t
end

type t = {
  forward: Forward.t;
  backward: Backward.t;
  sanitizers: Sanitizers.t;
  modes: ModeSet.t;
}
[@@deriving show]

val is_empty : with_modes:ModeSet.t -> t -> bool

val empty_model : t

val empty_skip_model : t (* Skips analysis *)

val obscure_model : t

val is_obscure : t -> bool

val remove_obscureness : t -> t

val remove_sinks : t -> t

val add_obscure_sink : resolution:Resolution.t -> call_target:Target.t -> t -> t

val join : t -> t -> t

(* A special case of join, only used for user-provided models. *)
val join_user_models : t -> t -> t

val widen : iteration:int -> previous:t -> next:t -> t

val reached_fixpoint : iteration:int -> previous:t -> next:t -> bool

val strip_for_callsite : t -> t

val apply_sanitizers : t -> t

val should_externalize : t -> bool

val to_json
  :  expand_overrides:Interprocedural.OverrideGraph.SharedMemory.t option ->
  is_valid_callee:
    (port:AccessPath.Root.t ->
    path:Abstract.TreeDomain.Label.path ->
    callee:Interprocedural.Target.t ->
    bool) ->
  filename_lookup:(Ast.Reference.t -> string option) option ->
  Target.t ->
  t ->
  Yojson.Safe.t

module WithTarget : sig
  type nonrec t = {
    model: t;
    target: Target.t;
  }
end

module WithCallTarget : sig
  type nonrec t = {
    model: t;
    call_target: CallGraph.CallTarget.t;
  }
end
