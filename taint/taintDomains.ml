(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module ForwardTaint = struct
  include AbstractSetDomain.Make(TaintSources)

end

module BackwardTaint = struct
  include AbstractSetDomain.Make(TaintSinks)

end


(* Used to infer which sources reach the exit points of a function. *)
module ForwardState =
  TaintAccessPathTree.Make
    (TaintAccessPathTree.WithChecks)
    (TaintAccessPath.Root)
    (ForwardTaint)


(* Used to infer which sinks are reached from parameters, as well as the
   taint-in-taint-out (TITO) using the special LocalReturn sink. *)
module BackwardState =
  TaintAccessPathTree.Make
    (TaintAccessPathTree.WithChecks)
    (TaintAccessPath.Root)
    (BackwardTaint)
