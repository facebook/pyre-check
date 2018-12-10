(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Analysis
open Taint
open Domains
open Core


let test_partition_call_map _ =
  let taint = ForwardTaint.singleton Sources.UserControlled in
  let call_taint1 =
    ForwardTaint.apply_call
      Location.Reference.any
      ~callees:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[AbstractTreeDomain.Label.create_name_field "a"]
      ~element:taint
  in
  let call_taint2 =
    ForwardTaint.apply_call
      Location.Reference.any
      ~callees:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[]
      ~element:taint
  in
  let joined = ForwardTaint.join call_taint1 call_taint2 in
  assert_equal
    ~cmp:(fun left right -> ForwardTaint.less_or_equal ~left ~right)
    ~printer:ForwardTaint.show
    ~msg:"call info not joined properly"
    joined call_taint2;
  assert_bool
    "joined should not be less or equal to non trivial access path in call trace"
    (ForwardTaint.less_or_equal ~left:joined ~right:call_taint1);
  let split partition =
    Map.Poly.find partition true |> Option.value ~default:ForwardTaint.bottom,
    Map.Poly.find partition false |> Option.value ~default:ForwardTaint.bottom
  in
  let matches, does_not_match =
    ForwardTaint.partition ForwardTaint.leaf ~f:((=) Sources.UserControlled) joined
    |> split
  in
  assert_equal
    ~msg:"does_not_match must be equal to bottom"
    ~printer:ForwardTaint.show
    does_not_match
    ForwardTaint.bottom;
  assert_equal
    ~msg:"matches must be equal to original"
    ~printer:ForwardTaint.show
    matches
    joined


let () =
  "test_taint_domain">:::[
    "partition_call_map">::test_partition_call_map;
  ]
  |> Test.run
