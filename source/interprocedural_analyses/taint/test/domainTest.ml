(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Taint
open Domains
open Core

let test_partition_call_map _ =
  let taint = ForwardTaint.singleton (Sources.NamedSource "UserControlled") in
  let call_taint1 =
    ForwardTaint.apply_call
      Location.WithModule.any
      ~callees:[]
      ~port:AccessPath.Root.LocalResult
      ~path:[Abstract.TreeDomain.Label.create_name_index "a"]
      ~element:taint
  in
  let call_taint2 =
    ForwardTaint.apply_call
      Location.WithModule.any
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
      ForwardTaint.leaf
      By
      ~f:(fun leaf -> Sources.equal leaf (Sources.NamedSource "UserControlled"))
      joined
    |> split
  in
  assert_equal
    ~msg:"does_not_match must be equal to bottom"
    ~printer:ForwardTaint.show
    does_not_match
    ForwardTaint.bottom;
  assert_equal ~msg:"matches must be equal to original" ~printer:ForwardTaint.show matches joined


let test_approximate_return_access_paths _ =
  let assert_approximate_return_access_paths ~expected ~cutoff_at tree =
    let compare left right =
      ForwardState.Tree.less_or_equal ~left ~right
      && ForwardState.Tree.less_or_equal ~left:right ~right:left
    in
    assert_equal
      ~cmp:compare
      ~printer:ForwardState.Tree.show
      expected
      (ForwardState.Tree.approximate_return_access_paths
         ~maximum_return_access_path_length:cutoff_at
         tree)
  in
  let create ~return_access_paths =
    ForwardState.Tree.create_leaf (ForwardTaint.singleton (Sources.NamedSource "Demo"))
    |> ForwardState.Tree.transform Features.ReturnAccessPathSet.Self Map ~f:(fun _ ->
           Features.ReturnAccessPathSet.of_list return_access_paths)
  in
  assert_approximate_return_access_paths
    ~expected:(create ~return_access_paths:[[Abstract.TreeDomain.Label.Index "a"]])
    ~cutoff_at:2
    (create ~return_access_paths:[[Abstract.TreeDomain.Label.Index "a"]]);
  assert_approximate_return_access_paths
    ~expected:
      (create
         ~return_access_paths:
           [[Abstract.TreeDomain.Label.Index "a"]; [Abstract.TreeDomain.Label.Index "b"]])
    ~cutoff_at:2
    (create
       ~return_access_paths:
         [[Abstract.TreeDomain.Label.Index "a"]; [Abstract.TreeDomain.Label.Index "b"]]);
  assert_approximate_return_access_paths
    ~expected:(create ~return_access_paths:[[]])
    ~cutoff_at:2
    (create
       ~return_access_paths:
         [
           [Abstract.TreeDomain.Label.Index "a"];
           [Abstract.TreeDomain.Label.Index "b"];
           [Abstract.TreeDomain.Label.Index "c"];
         ])


let () =
  "taint_domain"
  >::: [
         "partition_call_map" >:: test_partition_call_map;
         "approximate_return_access_paths" >:: test_approximate_return_access_paths;
       ]
  |> Test.run
