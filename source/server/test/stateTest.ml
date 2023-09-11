(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module BuildFailure = Server.ServerState.BuildFailure

let change_event path =
  PyrePath.create_absolute path
  |> SourcePath.create
  |> SourcePath.Event.(create ~kind:Kind.CreatedOrChanged)


let remove_event path =
  PyrePath.create_absolute path |> SourcePath.create |> SourcePath.Event.(create ~kind:Kind.Deleted)


let assert_events ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: SourcePath.Event.t list]
    ~printer:(fun events -> [%sexp_of: SourcePath.Event.t list] events |> Sexp.to_string_hum)
    expected
    actual


let assert_error_message ~context ~expected build_failure =
  let actual = BuildFailure.get_last_error_message build_failure in
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string option]
    ~printer:(fun message -> [%sexp_of: string option] message |> Sexp.to_string_hum)
    expected
    actual


let test_deferred_updates_empty context =
  assert_error_message ~context (BuildFailure.create ()) ~expected:None;
  assert_events (BuildFailure.create () |> BuildFailure.get_deferred_events) ~context ~expected:[]


let test_deferred_updates_add context =
  let build_failure = BuildFailure.create () in
  let event_a = change_event "/foo/a.py" in
  let event_b = remove_event "/foo/b.py" in
  let event_c = remove_event "/bar/c.py" in
  let event_d = change_event "/bar/d.py" in
  assert_error_message ~context build_failure ~expected:None;
  BuildFailure.update build_failure ~events:[event_a; event_b] ~error_message:"error0";
  assert_error_message ~context build_failure ~expected:(Some "error0");
  BuildFailure.update build_failure ~events:[event_c; event_d] ~error_message:"error1";
  assert_events
    (BuildFailure.get_deferred_events build_failure)
    ~context
    ~expected:[event_a; event_b; event_c; event_d];
  assert_error_message ~context build_failure ~expected:(Some "error1");
  ()


let test_deferred_updates_clear context =
  let build_failure = BuildFailure.create () in
  BuildFailure.update
    build_failure
    ~events:[change_event "/foo/a.py"; remove_event "/foo/b.py"]
    ~error_message:"error";
  assert_error_message ~context build_failure ~expected:(Some "error");
  BuildFailure.clear build_failure;
  assert_events (BuildFailure.get_deferred_events build_failure) ~context ~expected:[];
  assert_error_message ~context build_failure ~expected:None;
  ()


let () =
  "state"
  >::: [
         "deferred_updates_empty" >:: test_deferred_updates_empty;
         "deferred_updates_add" >:: test_deferred_updates_add;
         "deferred_updates_clear" >:: test_deferred_updates_clear;
       ]
  |> Test.run
