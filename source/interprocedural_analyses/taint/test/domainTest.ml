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
open Test
open Analysis

let test_partition_call_map context =
  let global_resolution =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution
  in
  let resolution =
    TypeCheck.resolution
      global_resolution
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)
  in
  let taint =
    ForwardTaint.singleton CallInfo.declaration (Sources.NamedSource "UserControlled") Frame.initial
  in
  let callee =
    Some
      (Interprocedural.Target.Method { class_name = "test.Foo"; method_name = "bar"; kind = Normal })
  in
  let call_taint1 =
    ForwardTaint.apply_call
      ~resolution
      ~location:Location.WithModule.any
      ~callee
      ~arguments:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~element:taint
      ~is_self_call:false
      ~caller_class_interval:Interprocedural.ClassIntervalSet.top
      ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
  in
  let call_taint2 =
    ForwardTaint.apply_call
      ~resolution
      ~location:Location.WithModule.any
      ~callee
      ~arguments:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~element:taint
      ~is_self_call:false
      ~caller_class_interval:Interprocedural.ClassIntervalSet.top
      ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
  in
  let joined = ForwardTaint.join call_taint1 call_taint2 in
  assert_equal
    ~cmp:(fun left right -> ForwardTaint.less_or_equal ~left ~right)
    ~printer:ForwardTaint.show
    ~msg:"call info not joined properly"
    joined
    call_taint2;
  assert_bool
    "joined should not be less or equal to non trivial access path in call trace"
    (ForwardTaint.less_or_equal ~left:joined ~right:call_taint1);
  let split partition =
    ( Map.Poly.find partition true |> Option.value ~default:ForwardTaint.bottom,
      Map.Poly.find partition false |> Option.value ~default:ForwardTaint.bottom )
  in
  let matches, does_not_match =
    ForwardTaint.partition
      ForwardTaint.kind
      By
      ~f:(fun kind -> Sources.equal kind (Sources.NamedSource "UserControlled"))
      joined
    |> split
  in
  assert_equal
    ~msg:"does_not_match must be equal to bottom"
    ~printer:ForwardTaint.show
    does_not_match
    ForwardTaint.bottom;
  assert_equal ~msg:"matches must be equal to original" ~printer:ForwardTaint.show matches joined


let test_call_info_interval _ =
  let assert_equal_interval ~actual ~expected =
    assert_equal ~printer:CallInfoIntervals.show actual expected
  in
  assert_equal_interval
    ~actual:(CallInfoIntervals.join CallInfoIntervals.top CallInfoIntervals.bottom)
    ~expected:CallInfoIntervals.top;
  assert_equal_interval
    ~actual:(CallInfoIntervals.meet CallInfoIntervals.top CallInfoIntervals.bottom)
    ~expected:CallInfoIntervals.bottom;
  assert_equal
    (CallInfoIntervals.less_or_equal ~left:CallInfoIntervals.top ~right:CallInfoIntervals.bottom)
    false;
  assert_equal
    (CallInfoIntervals.less_or_equal ~left:CallInfoIntervals.bottom ~right:CallInfoIntervals.top)
    true


let () =
  "taint_domain"
  >::: [
         "partition_call_map" >:: test_partition_call_map;
         "call_info_interval" >:: test_call_info_interval;
       ]
  |> Test.run
