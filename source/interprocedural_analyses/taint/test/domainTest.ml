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
  let taint = ForwardTaint.singleton (Sources.NamedSource "UserControlled") Frame.initial in
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
      ~path:[]
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
    ForwardTaint.singleton (Sources.NamedSource "Demo") Frame.initial
    |> ForwardState.Tree.create_leaf
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


let test_sanitize _ =
  let assert_sanitize_equal ~expected actual =
    assert_equal
      ~cmp:Taint.Domains.Sanitize.equal
      ~printer:Taint.Domains.Sanitize.show
      actual
      expected
  in
  let source_a = Sources.NamedSource "a" in
  let source_a_set = Sources.Set.singleton source_a in
  let source_b = Sources.NamedSource "b" in
  let source_b_set = Sources.Set.singleton source_b in
  let source_ab_set = Sources.Set.of_list [source_a; source_b] in
  let sink_a = Sinks.NamedSink "a" in
  let sink_a_set = Sinks.Set.singleton sink_a in
  let sink_b = Sinks.NamedSink "b" in
  let sink_b_set = Sinks.Set.singleton sink_b in
  let sink_ab_set = Sinks.Set.of_list [sink_a; sink_b] in

  (* Test join *)
  assert_sanitize_equal
    (Sanitize.join
       Sanitize.bottom
       { Sanitize.sources = Some All; sinks = Some All; tito = Some All })
    ~expected:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_sanitize_equal
    (Sanitize.join
       { Sanitize.sources = Some All; sinks = Some All; tito = Some All }
       Sanitize.bottom)
    ~expected:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = Some (Specific source_a_set);
         sinks = Some All;
         tito =
           Some
             (Specific { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_a_set });
       }
       { Sanitize.sources = Some All; sinks = Some (Specific sink_b_set); tito = Some All })
    ~expected:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = Some (Specific source_a_set);
         sinks = Some (Specific sink_b_set);
         tito =
           Some
             (Specific { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_a_set });
       }
       {
         Sanitize.sources = Some (Specific source_b_set);
         sinks = Some (Specific sink_a_set);
         tito =
           Some
             (Specific { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
       })
    ~expected:
      {
        Sanitize.sources = Some (Specific source_ab_set);
        sinks = Some (Specific sink_ab_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_ab_set; sanitized_tito_sinks = sink_ab_set });
      };

  (* Tess less_or_equal *)
  let assert_less_or_equal ~left ~right =
    assert_equal
      ~cmp:(fun left right -> Sanitize.less_or_equal ~left ~right)
      ~printer:Sanitize.show
      ~msg:"left is not less or equal than right"
      left
      right
  in
  let assert_not_less_or_equal ~left ~right =
    assert_equal
      ~cmp:(fun left right -> not (Sanitize.less_or_equal ~left ~right))
      ~printer:Sanitize.show
      ~msg:"left is less or equal than right"
      left
      right
  in
  assert_less_or_equal
    ~left:Sanitize.bottom
    ~right:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_not_less_or_equal
    ~left:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
    ~right:Sanitize.bottom;
  assert_less_or_equal
    ~left:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
    ~right:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_less_or_equal
    ~left:{ Sanitize.sources = None; sinks = Some All; tito = None }
    ~right:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_not_less_or_equal
    ~left:{ Sanitize.sources = None; sinks = Some All; tito = None }
    ~right:{ Sanitize.sources = Some All; sinks = None; tito = Some All };
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (Specific source_a_set);
        sinks = Some (Specific sink_a_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All };
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (Specific source_a_set);
        sinks = Some (Specific sink_b_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:
      {
        Sanitize.sources = Some (Specific source_ab_set);
        sinks = Some (Specific sink_ab_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_ab_set; sanitized_tito_sinks = sink_ab_set });
      };
  assert_not_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (Specific source_a_set);
        sinks = Some (Specific sink_b_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:
      {
        Sanitize.sources = Some (Specific source_ab_set);
        sinks = Some (Specific sink_ab_set);
        tito =
          Some
            (Specific { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_ab_set });
      }


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
         "approximate_return_access_paths" >:: test_approximate_return_access_paths;
         "sanitize" >:: test_sanitize;
         "call_info_interval" >:: test_call_info_interval;
       ]
  |> Test.run
