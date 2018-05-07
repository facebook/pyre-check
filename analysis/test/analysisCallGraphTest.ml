(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Analysis
open Ast
open Expression


let test_create _ =
  let call_graph = CallGraph.create () in
  let (module Handler: CallGraph.Handler) = CallGraph.handler call_graph in

  Handler.register_overload ~access:(Access.create "one") ~overload:(Access.create "one");
  Handler.register_overload ~access:(Access.create "one") ~overload:(Access.create "two");
  Handler.register_overload ~access:(Access.create "two") ~overload:(Access.create "three");

  let { CallGraph.overloads } = call_graph in
  assert_equal 2 (Access.Table.length overloads)


let () =
  "callGraph">:::[
    "create">::test_create;
  ]
  |> run_test_tt_main
