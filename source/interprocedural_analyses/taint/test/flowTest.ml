(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Taint
open Domains
open Core

let test_no_errors _ =
  let open Flow in
  let source_tree_a =
    ForwardTaint.singleton (Sources.NamedSource "Demo") Frame.initial
    |> ForwardState.Tree.create_leaf
    |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Index "a"]
  in
  let source_tree_b =
    ForwardTaint.singleton (Sources.NamedSource "Test") Frame.initial
    |> ForwardState.Tree.create_leaf
    |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Index "b"]
  in
  let sink_tree_a =
    BackwardTaint.singleton (Sinks.NamedSink "Test") Frame.initial
    |> BackwardState.Tree.create_leaf
    |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.Index "a"]
  in
  let sink_tree_b =
    BackwardTaint.singleton (Sinks.NamedSink "Demo") Frame.initial
    |> BackwardState.Tree.create_leaf
    |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.Index "b"]
  in
  let assert_no_errors ~source_tree ~sink_tree =
    let location =
      Location.with_module
        ~qualifier:Reference.empty
        (Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos)
    in
    let define =
      Statement.Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
      |> Node.create_with_default_location
    in
    let errors =
      generate_source_sink_matches ~location ~source_tree ~sink_tree
      |> generate_issues ~define
      |> List.map ~f:generate_error
    in
    assert_equal
      ~msg:"Errors"
      ~printer:(fun errors -> Sexp.to_string [%message (errors : Interprocedural.Error.t list)])
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
  let source_tree ~field ~source =
    ForwardTaint.singleton (Sources.NamedSource source) Frame.initial
    |> ForwardState.Tree.create_leaf
    |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Index field]
  in
  let sink_tree ~field ~sink =
    BackwardTaint.singleton (Sinks.NamedSink sink) Frame.initial
    |> BackwardState.Tree.create_leaf
    |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.Index field]
  in

  let source_tree_a = source_tree ~source:"UserControlled" ~field:"a" in
  let source_tree_b = source_tree ~source:"Test" ~field:"b" in
  let source_tree_c = source_tree ~source:"Demo" ~field:"a" in
  let sink_tree_a = sink_tree ~sink:"RemoteCodeExecution" ~field:"a" in
  let sink_tree_b = sink_tree ~sink:"Test" ~field:"b" in
  let sink_tree_c = sink_tree ~sink:"Demo" ~field:"a" in
  let sink_tree_d = sink_tree ~sink:"Test" ~field:"a" in
  let assert_errors ~source_tree ~sink_tree codes =
    let location =
      Location.with_module
        ~qualifier:Reference.empty
        (Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos)
    in
    let define =
      Statement.Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
      |> Node.create_with_default_location
    in
    let errors =
      generate_source_sink_matches ~location ~source_tree ~sink_tree
      |> generate_issues ~define
      |> List.map ~f:generate_error
    in
    assert_equal
      ~msg:"Error"
      ~printer:(List.to_string ~f:Int.to_string)
      codes
      (List.map errors ~f:(fun error -> Interprocedural.Error.code error))
  in
  assert_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_a [5001];
  assert_errors ~source_tree:source_tree_b ~sink_tree:sink_tree_b [5002];
  assert_errors ~source_tree:source_tree_c ~sink_tree:sink_tree_c [5009; 6001];
  assert_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_d [5002];
  ()


let () = "taint_flow" >::: ["no_errors" >:: test_no_errors; "errors" >:: test_errors] |> Test.run
