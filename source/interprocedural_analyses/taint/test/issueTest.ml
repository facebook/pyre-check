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
  let open Issue in
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
        ~module_reference:Reference.empty
        (Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos)
    in
    let define =
      Statement.Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
      |> Node.create_with_default_location
    in
    let candidates = Candidates.create () in
    let () =
      Candidates.check_flow
        candidates
        ~location
        ~sink_handle:SinkHandle.Return
        ~source_tree
        ~sink_tree
    in
    let errors = Candidates.generate_issues candidates ~define |> List.map ~f:to_error in
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
  let open Issue in
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
        ~module_reference:Reference.empty
        (Location.create ~start:Lexing.dummy_pos ~stop:Lexing.dummy_pos)
    in
    let define =
      Statement.Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
      |> Node.create_with_default_location
    in
    let candidates = Candidates.create () in
    let () =
      Candidates.check_flow
        candidates
        ~location
        ~sink_handle:SinkHandle.Return
        ~source_tree
        ~sink_tree
    in
    let errors = Candidates.generate_issues candidates ~define |> List.map ~f:to_error in
    assert_equal
      ~msg:"Error"
      ~printer:(List.to_string ~f:Int.to_string)
      codes
      (List.map errors ~f:(fun error -> Interprocedural.Error.code error))
  in
  assert_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_a [5001];
  assert_errors ~source_tree:source_tree_b ~sink_tree:sink_tree_b [5002];
  assert_errors ~source_tree:source_tree_c ~sink_tree:sink_tree_c [6001; 5009];
  assert_errors ~source_tree:source_tree_a ~sink_tree:sink_tree_d [5002];
  ()


let test_canonical_location _ =
  let assert_canonical_location ~set ~expected =
    let define =
      Statement.Define.create_toplevel ~unbound_names:[] ~qualifier:None ~statements:[]
      |> Node.create_with_default_location
    in
    let locations =
      List.fold
        ~init:Issue.LocationSet.empty
        ~f:(fun set location -> Issue.LocationSet.add location set)
        set
    in
    let issue =
      {
        Issue.flow = Issue.Flow.bottom;
        handle =
          {
            code = 1000;
            callable = Interprocedural.Target.create define;
            sink = Issue.SinkHandle.Return;
          };
        locations;
        define;
      }
    in
    let actual = Issue.canonical_location issue in
    assert_equal expected actual
  in
  let create_location
      ?(qualifier = "a")
      ?(start_line = 0)
      ?(start_column = 0)
      ?(stop_line = 0)
      ?(stop_column = 1)
      ()
    =
    Location.with_module
      ~module_reference:(Reference.create qualifier)
      {
        start = { line = start_line; column = start_column };
        stop = { line = stop_line; column = stop_column };
      }
  in
  assert_canonical_location ~set:[create_location ()] ~expected:(create_location ());
  assert_canonical_location
    ~set:[create_location (); create_location ()]
    ~expected:(create_location ());
  assert_canonical_location
    ~set:
      [
        create_location ~qualifier:"a" ();
        create_location ~qualifier:"b" ();
        create_location ~qualifier:"c" ();
      ]
    ~expected:(create_location ~qualifier:"a" ());
  assert_canonical_location
    ~set:
      [
        create_location ~qualifier:"b.a" ();
        create_location ~qualifier:"a.z" ();
        create_location ~qualifier:"a.y" ();
      ]
    ~expected:(create_location ~qualifier:"a.y" ());
  assert_canonical_location
    ~set:
      [
        create_location ~start_line:2 ();
        create_location ~start_line:1 ();
        create_location ~start_line:0 ();
      ]
    ~expected:(create_location ~start_line:0 ());
  assert_canonical_location
    ~set:
      [
        create_location ~start_line:2 ~start_column:6 ();
        create_location ~start_line:2 ~start_column:2 ();
        create_location ~start_line:2 ~start_column:4 ();
      ]
    ~expected:(create_location ~start_line:2 ~start_column:2 ());
  assert_canonical_location
    ~set:
      [
        create_location ~qualifier:"c" ~start_line:4 ~start_column:3 ~stop_line:10 ~stop_column:9 ();
        create_location ~qualifier:"b" ~start_line:2 ~start_column:10 ~stop_line:8 ~stop_column:8 ();
        create_location ~qualifier:"e" ~start_line:20 ~start_column:2 ~stop_line:4 ~stop_column:6 ();
        create_location ~qualifier:"b" ~start_line:2 ~start_column:7 ~stop_line:6 ~stop_column:7 ();
        create_location ~qualifier:"b" ~start_line:2 ~start_column:20 ~stop_line:2 ~stop_column:5 ();
        create_location ~qualifier:"c" ~start_line:4 ~start_column:4 ~stop_line:4 ~stop_column:4 ();
        create_location ~qualifier:"b" ~start_line:4 ~start_column:7 ~stop_line:6 ~stop_column:3 ();
        create_location ~qualifier:"b" ~start_line:2 ~start_column:7 ~stop_line:98 ~stop_column:2 ();
        create_location ~qualifier:"b" ~start_line:2 ~start_column:7 ~stop_line:6 ~stop_column:10 ();
      ]
    ~expected:
      (create_location ~qualifier:"b" ~start_line:2 ~start_column:7 ~stop_line:6 ~stop_column:7 ());
  ()


let () =
  "taint_flow"
  >::: [
         "no_errors" >:: test_no_errors;
         "errors" >:: test_errors;
         "canonical_location" >:: test_canonical_location;
       ]
  |> Test.run
