(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module ForwardTaint = AbstractSetDomain.Make(TaintSources)
module BackwardTaint = AbstractSetDomain.Make(TaintSinks)


(* Placeholder for representing parameters, locals, and special return value. *)
module Root = struct
  type t =
    | LocalResult  (* Special root representing the return value location. *)
    | Parameter of string
    | Var of string
  [@@deriving compare, sexp, show, hash]
end


(* Used to infer which sources reach the exit points of a function. *)
module ForwardState =
  AccessPathTree.Make
    (AccessPathTree.WithChecks)
    (Root)
    (ForwardTaint)


(* Used to infer which sinks are reached from parameters, as well as the
   taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState =
  AccessPathTree.Make
    (AccessPathTree.WithChecks)
    (Root)
    (BackwardTaint)
