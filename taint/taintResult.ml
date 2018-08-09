(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module Error = Interprocedural.Error
open TaintDomains


module Forward = struct
  type model = {
    source_taint: ForwardState.t;
  }
  [@@deriving show, sexp]

  let empty = {
    source_taint = ForwardState.empty;
  }

  let obscure = empty

  let join { source_taint = left; } { source_taint = right; } =
    { source_taint = ForwardState.join left right }

  let widen
      ~iteration
      ~previous:{ source_taint = previous; }
      ~next:{ source_taint = next; } =
    { source_taint = ForwardState.widen ~iteration ~previous ~next }

  let reached_fixpoint
      ~iteration:_
      ~previous:{ source_taint = previous; }
      ~next:{ source_taint = next; } =
    ForwardState.less_or_equal ~left:next ~right:previous

end


module Backward = struct
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving show, sexp]

  let empty = {
    sink_taint = BackwardState.empty;
    taint_in_taint_out = BackwardState.empty;
  }

  let obscure = empty

  let join
      { sink_taint = sink_taint_left; taint_in_taint_out = tito_left; }
      { sink_taint = sink_taint_right; taint_in_taint_out = tito_right; } =
    {
      sink_taint = BackwardState.join sink_taint_left sink_taint_right;
      taint_in_taint_out = BackwardState.join tito_left tito_right;
    }

  let widen
      ~iteration
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous; }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next; } =
    let sink_taint =
      BackwardState.widen ~iteration ~previous:sink_taint_previous ~next:sink_taint_next in
    let taint_in_taint_out =
      BackwardState.widen ~iteration ~previous:tito_previous ~next:tito_next in
    { sink_taint; taint_in_taint_out; }

  let reached_fixpoint
      ~iteration:_
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous; }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next; } =
    BackwardState.less_or_equal ~left:sink_taint_next ~right:sink_taint_previous
    && BackwardState.less_or_equal ~left:tito_next ~right:tito_previous

end


type call_model = {
  forward: Forward.model;
  backward: Backward.model;
}
[@@deriving show, sexp]


type result = Error.t list


module ResultArgument = struct

  let name = "taint analysis"

  type nonrec result = result
  type nonrec call_model = call_model

  let show_call_model = show_call_model

  let obscure_model = {
    forward = Forward.obscure;
    backward = Backward.obscure;
  }

  let empty_model = {
    forward = Forward.empty;
    backward = Backward.empty;
  }

  let join ~iteration:_ left right =
    {
      forward = Forward.join left.forward right.forward;
      backward = Backward.join left.backward right.backward;
    }

  let widen ~iteration ~previous ~next =
    {
      forward = Forward.widen ~iteration ~previous:previous.forward ~next:next.forward;
      backward = Backward.widen ~iteration ~previous:previous.backward ~next:next.backward;
    }


  let get_errors result = result

  let reached_fixpoint ~iteration ~previous ~next =
    Forward.reached_fixpoint ~iteration ~previous:previous.forward ~next:next.forward
    && Backward.reached_fixpoint ~iteration ~previous:previous.backward ~next:next.backward

  let summaries _callable _result _model =
    []
end


include Interprocedural.Result.Make(ResultArgument)
