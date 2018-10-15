(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Domains


module Backward : sig
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving show, sexp]

  val empty: model
end

module Forward : sig
  type model = {
    source_taint: ForwardState.t;
  }
  [@@deriving show, sexp]

  val empty: model
end

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
}
[@@deriving show, sexp]

val empty_model: call_model

type result = Flow.issue list

include Interprocedural.Result.ANALYSIS_RESULT_WITH_REGISTRATION
  with type result := result
   and type call_model := call_model
