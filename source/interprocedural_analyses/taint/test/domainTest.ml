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

let test_partition_call_map context =
  let pyre_api =
    ScratchProject.setup ~context []
    |> ScratchProject.pyre_pysa_read_only_api
    |> Interprocedural.PyrePysaApi.ReadOnly.from_pyre1_api
  in
  let pyre_in_context =
    Interprocedural.PyrePysaApi.InContext.create_at_function_scope
      pyre_api
      ~module_qualifier:!&"test"
      ~define_name:!&"test.__toplevel__"
  in
  let taint =
    ForwardTaint.singleton CallInfo.declaration (Sources.NamedSource "UserControlled") Frame.initial
  in
  let callee =
    Target.Regular.Method { class_name = "test.Foo"; method_name = "bar"; kind = Normal }
    |> Target.from_regular
  in
  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadWrite.empty ()
        |> Interprocedural.CallablesSharedMemory.ReadOnly.read_only)
      ()
  in
  let call_taint1 =
    ForwardTaint.apply_call
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~call_site:CallSite.any
      ~location:Location.any
      ~callee
      ~arguments:[]
      ~port:Analysis.TaintAccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~is_class_method:false
      ~is_static_method:false
      ~call_info_intervals:Domains.ClassIntervals.top
      taint
  in
  let call_taint2 =
    ForwardTaint.apply_call
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~call_site:CallSite.any
      ~location:Location.any
      ~callee
      ~arguments:[]
      ~port:Analysis.TaintAccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~is_class_method:false
      ~is_static_method:false
      ~call_info_intervals:Domains.ClassIntervals.top
      taint
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


let () = "taint_domain" >::: ["partition_call_map" >:: test_partition_call_map] |> Test.run
