(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Taint
open Domains
open Core


let is_user_controlled = (=) Sources.UserControlled
let is_RCE = (=) Sinks.RemoteCodeExecution


let source_taint = ForwardTaint.of_list [ Sources.TestSource; Sources.UserControlled; ]
let sink_taint = BackwardTaint.of_list [ Sinks.TestSink; Sinks.RemoteCodeExecution; ]


let test_partition_match_all _ =
  let open Flow in
  let flows = [ { source_taint; sink_taint; } ] in
  let { matched; rest; } = partition_flows flows in
  assert_equal
    ~msg:"Matching"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    flows
    matched;
  assert_equal
    ~msg:"Rest"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    []
    rest


let test_partition_match_some_sources _ =
  let open Flow in
  let flows = [ { source_taint; sink_taint; } ] in
  let { matched; rest; } = partition_flows ~sources:is_user_controlled flows in
  assert_equal
    ~msg:"Matching"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint = ForwardTaint.singleton Sources.UserControlled;
        sink_taint;
      };
    ]
    matched;
  assert_equal
    ~msg:"Rest"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint = ForwardTaint.singleton Sources.TestSource;
        sink_taint;
      };
    ]
    rest


let test_partition_match_some_sinks _ =
  let open Flow in
  let flows = [ { source_taint; sink_taint; } ] in
  let { matched; rest; } = partition_flows ~sinks:is_RCE flows in
  assert_equal
    ~msg:"Matching"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint;
        sink_taint = BackwardTaint.singleton Sinks.RemoteCodeExecution;
      };
    ]
    matched;
  assert_equal
    ~msg:"Rest"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint;
        sink_taint = BackwardTaint.singleton Sinks.TestSink;
      };
    ]
    rest


let test_partition_match_some_sinks_and_sources _ =
  let open Flow in
  let flows = [ { source_taint; sink_taint; } ] in
  let { matched; rest; } = partition_flows ~sources:is_user_controlled ~sinks:is_RCE flows in
  assert_equal
    ~msg:"Matching"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint = ForwardTaint.singleton Sources.UserControlled;
        sink_taint = BackwardTaint.singleton Sinks.RemoteCodeExecution;
      };
    ]
    matched;
  assert_equal
    ~msg:"Rest"
    ~printer:(fun taint -> Sexp.to_string [%message (taint: Flow.flow list)])
    [
      {
        source_taint = ForwardTaint.singleton Sources.TestSource;
        sink_taint = BackwardTaint.singleton Sinks.RemoteCodeExecution;
      };
      {
        source_taint;
        sink_taint = BackwardTaint.singleton Sinks.TestSink;
      };
    ]
    rest


let test_no_errors _ =
  let open Flow in
  let source_tree_a =
    ForwardTaint.singleton Sources.UserControlled
    |> ForwardState.make_leaf
    |> ForwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "a")]
  in
  let source_tree_b =
    ForwardTaint.singleton Sources.TestSource
    |> ForwardState.make_leaf
    |> ForwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "b")]
  in
  let sink_tree_a =
    BackwardTaint.singleton Sinks.TestSink
    |> BackwardState.make_leaf
    |> BackwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "a")]
  in
  let sink_tree_b =
    BackwardTaint.singleton Sinks.RemoteCodeExecution
    |> BackwardState.make_leaf
    |> BackwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "b")]
  in
  let assert_no_errors ~source_tree ~sink_tree =
    let location = Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos in
    let define =
      Statement.Define.create_toplevel ~qualifier:[] ~statements:[]
      |> Node.create_with_default_location
    in
    let errors =
      generate_source_sink_matches ~location ~source_tree ~sink_tree
      |> generate_errors ~define
    in
    assert_equal
      ~msg:"Errors"
      ~printer:(fun errors -> Sexp.to_string [%message (errors: Interprocedural.Error.t list)])
      []
      errors
  in
  assert_no_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_b;
  assert_no_errors ~source_tree:source_tree_b ~sink_tree:sink_tree_a;
  assert_no_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_a;
  assert_no_errors ~source_tree:source_tree_b ~sink_tree:sink_tree_b;
  ()


let test_errors _ =
  let open Flow in
  let source_tree_a =
    ForwardTaint.singleton Sources.UserControlled
    |> ForwardState.make_leaf
    |> ForwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "a")]
  in
  let source_tree_b =
    ForwardTaint.singleton Sources.TestSource
    |> ForwardState.make_leaf
    |> ForwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "b")]
  in
  let sink_tree_a =
    BackwardTaint.singleton Sinks.RemoteCodeExecution
    |> BackwardState.make_leaf
    |> BackwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "a")]
  in
  let sink_tree_b =
    BackwardTaint.singleton Sinks.TestSink
    |> BackwardState.make_leaf
    |> BackwardState.make_tree [AccessPathTree.Label.Field (Identifier.create "b")]
  in
  let assert_error ~source_tree ~sink_tree code =
    let location = Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos in
    let define =
      Statement.Define.create_toplevel ~qualifier:[] ~statements:[]
      |> Node.create_with_default_location
    in
    let errors =
      generate_source_sink_matches ~location ~source_tree ~sink_tree
      |> generate_errors ~define
    in
    assert_equal
      ~msg:"Error"
      ~printer:Int.to_string
      code
      Interprocedural.Error.((List.hd_exn errors).kind.code)
  in
  assert_error ~source_tree:source_tree_a ~sink_tree:sink_tree_a 5001;
  assert_error ~source_tree:source_tree_b ~sink_tree:sink_tree_b 5002;
  ()


let () =
  Scheduler.mock () |> ignore;
  "test_taint_flow">:::[
    "partition_match_all">::test_partition_match_all;
    "partition_match_some_sources">::test_partition_match_some_sources;
    "partition_match_some_sinks">::test_partition_match_some_sinks;
    "partition_match_some_sinks_and_sources">::test_partition_match_some_sinks_and_sources;
    "test_no_errors">::test_no_errors;
    "test_errors">::test_errors;
  ]
  |> run_test_tt_main
