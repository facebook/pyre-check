(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open Ast
open Taint
open Domains
open Core


let test_partition_call_map _ =
  let taint = ForwardTaint.singleton Sources.UserControlled in
  let call_taint =
    ForwardTaint.apply_call
      Location.Reference.any
      ~callees:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[]
      ~path_element:ForwardTaint.bottom
      ~element:taint
  in
  let matches, does_not_match =
    ForwardTaint.partition_tf ~f:((=) Sources.UserControlled) call_taint
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
    call_taint


let () =
  "test_taint_domain">:::[
    "partition_call_map">::test_partition_call_map;
  ]
  |> Test.run
