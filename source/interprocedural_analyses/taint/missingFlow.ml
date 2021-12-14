(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression
open Domains

let unknown_callee ~location ~call =
  let callee =
    match call with
    | Expression.Call { callee; _ } -> callee
    | _ -> Node.create ~location:(Location.strip_module location) call
  in
  Interprocedural.Target.create_function
    (Reference.create
       (Format.asprintf "%a:%a" Location.WithModule.pp location Expression.pp callee))


let register_unknown_callee_model callable =
  (* Add a model with sinks on *args and **kwargs. *)
  let sink_leaf =
    BackwardTaint.singleton (Sinks.NamedSink "UnknownCallee") Frame.initial
    |> BackwardState.Tree.create_leaf
  in
  let sink_taint =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      sink_leaf
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         sink_leaf
  in
  (* Add taint-in-taint-out for all parameters. *)
  let local_return =
    BackwardTaint.singleton Sinks.LocalReturn Frame.initial |> BackwardState.Tree.create_leaf
  in
  let taint_in_taint_out =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      local_return
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         local_return
  in
  Interprocedural.FixpointState.add_predefined
    Interprocedural.FixpointState.Epoch.predefined
    callable
    (Interprocedural.AnalysisResult.make_model
       TaintResult.kind
       {
         forward = Model.Forward.empty;
         backward = { sink_taint; taint_in_taint_out };
         sanitizers = Model.Sanitizers.empty;
         modes = Model.ModeSet.singleton Model.Mode.SkipAnalysis;
       })
