(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural
open CallGraph
open CallGraphBuilder
module AccessPath = Analysis.TaintAccessPath

module HigherOrderCallGraphForTest = struct
  type t = {
    returned_callables: CallTarget.Set.t;
    call_graph: DefineCallGraphForTest.t;
  }
  [@@deriving equal, show]

  let from_actual { HigherOrderCallGraph.returned_callables; call_graph; _ } =
    { returned_callables; call_graph = DefineCallGraph.for_test call_graph }


  module Expected = struct
    type t = {
      returned_callables: CallTarget.t list;
      call_graph: (string * ExpressionCallees.t) list;
    }
  end

  let from_expected { Expected.call_graph; returned_callables } =
    {
      call_graph = DefineCallGraphForTest.from_expected call_graph;
      returned_callables = CallTarget.Set.of_list returned_callables;
    }
end

let create_parameterized_target ~regular ~parameters =
  let parameters =
    Core.List.map parameters ~f:(fun (root, target) -> root, Target.ParameterValue.create target)
  in
  Target.Parameterized { regular; parameters = Target.ParameterMap.of_alist_exn parameters }


(* Like `create_parameterized_target`, but each parameter is given as a `Target.ParameterValue.t`,
   so tests can set the `implicit_receiver` flag (e.g. for captured bound methods). *)
let create_parameterized_target_with_values ~regular ~parameters =
  Target.Parameterized { regular; parameters = Target.ParameterMap.of_alist_exn parameters }


let create_positional_parameter ?(positional_only = false) position name =
  AccessPath.Root.PositionalParameter { position; name; positional_only }
