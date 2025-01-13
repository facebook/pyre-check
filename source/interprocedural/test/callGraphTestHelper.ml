(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Interprocedural
open CallGraph

let parse_call_graph_location location =
  let parse_position position =
    let line_and_column = String.split ~on:':' position in
    {
      Location.line = Int.of_string (List.nth_exn line_and_column 0);
      column = Int.of_string (List.nth_exn line_and_column 1);
    }
  in
  let positions = String.split ~on:'-' location in
  {
    Location.start = parse_position (List.nth_exn positions 0);
    stop = parse_position (List.nth_exn positions 1);
  }


let parse_define_call_graph =
  List.fold
    ~init:DefineCallGraph.empty
    ~f:(fun call_graph_of_define (location, expression_identifier, callees) ->
      DefineCallGraph.add_callees
        call_graph_of_define
        ~expression_identifier
        ~location:(parse_call_graph_location location)
        ~callees)


let parse_define_call_graph_for_test =
  List.fold ~init:DefineCallGraphForTest.empty ~f:(fun call_graph_of_define (location, callees) ->
      DefineCallGraphForTest.add
        call_graph_of_define
        ~location:(parse_call_graph_location location)
        ~callees)


module HigherOrderCallGraphForTest = struct
  type t = {
    returned_callables: CallTarget.Set.t;
    call_graph: DefineCallGraphForTest.t;
  }
  [@@deriving eq, show]

  let from_actual { HigherOrderCallGraph.returned_callables; call_graph; _ } =
    { returned_callables; call_graph = DefineCallGraph.for_test call_graph }


  module Expected = struct
    type t = {
      returned_callables: CallTarget.t list;
      call_graph: (string * LocationCallees.t) list;
    }
  end

  let from_expected { Expected.call_graph; returned_callables } =
    {
      call_graph = parse_define_call_graph_for_test call_graph;
      returned_callables = CallTarget.Set.of_list returned_callables;
    }
end

let create_parameterized_target ~regular ~parameters =
  Target.Parameterized { regular; parameters = Target.ParameterMap.of_alist_exn parameters }


let create_positional_parameter ?(positional_only = false) position name =
  AccessPath.Root.PositionalParameter { position; name; positional_only }


let test_py = "test.py"

let test_module_name = Reference.create (String.chop_suffix_exn test_py ~suffix:".py")

let setup ~context ~source =
  let project = Test.ScratchProject.setup ~context [test_py, source] in
  let pyre_api = Test.ScratchProject.pyre_pysa_read_only_api project in
  let test_source = TestHelper.source_from_qualifier ~pyre_api test_module_name in
  test_source, pyre_api, Test.ScratchProject.configuration_of project
