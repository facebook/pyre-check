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
       { Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito })
    ~expected:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_sanitize_equal
    (Sanitize.join
       { Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito }
       Sanitize.bottom)
    ~expected:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = Some (SpecificSources source_a_set);
         sinks = Some AllSinks;
         tito =
           Some
             (SpecificTito
                { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_a_set });
       }
       {
         Sanitize.sources = Some AllSources;
         sinks = Some (SpecificSinks sink_b_set);
         tito = Some AllTito;
       })
    ~expected:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = Some (SpecificSources source_a_set);
         sinks = Some (SpecificSinks sink_b_set);
         tito =
           Some
             (SpecificTito
                { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_a_set });
       }
       {
         Sanitize.sources = Some (SpecificSources source_b_set);
         sinks = Some (SpecificSinks sink_a_set);
         tito =
           Some
             (SpecificTito
                { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
       })
    ~expected:
      {
        Sanitize.sources = Some (SpecificSources source_ab_set);
        sinks = Some (SpecificSinks sink_ab_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_ab_set; sanitized_tito_sinks = sink_ab_set });
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
    ~right:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_not_less_or_equal
    ~left:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito }
    ~right:Sanitize.bottom;
  assert_less_or_equal
    ~left:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito }
    ~right:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_less_or_equal
    ~left:{ Sanitize.sources = None; sinks = Some AllSinks; tito = None }
    ~right:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_not_less_or_equal
    ~left:{ Sanitize.sources = None; sinks = Some AllSinks; tito = None }
    ~right:{ Sanitize.sources = Some AllSources; sinks = None; tito = Some AllTito };
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (SpecificSources source_a_set);
        sinks = Some (SpecificSinks sink_a_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:{ Sanitize.sources = Some AllSources; sinks = Some AllSinks; tito = Some AllTito };
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (SpecificSources source_a_set);
        sinks = Some (SpecificSinks sink_b_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:
      {
        Sanitize.sources = Some (SpecificSources source_ab_set);
        sinks = Some (SpecificSinks sink_ab_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_ab_set; sanitized_tito_sinks = sink_ab_set });
      };
  assert_not_less_or_equal
    ~left:
      {
        Sanitize.sources = Some (SpecificSources source_a_set);
        sinks = Some (SpecificSinks sink_b_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_a_set; sanitized_tito_sinks = sink_b_set });
      }
    ~right:
      {
        Sanitize.sources = Some (SpecificSources source_ab_set);
        sinks = Some (SpecificSinks sink_ab_set);
        tito =
          Some
            (SpecificTito
               { sanitized_tito_sources = source_b_set; sanitized_tito_sinks = sink_ab_set });
      }


let () =
  "taint_domain"
  >::: [
         "partition_call_map" >:: test_partition_call_map;
         "approximate_return_access_paths" >:: test_approximate_return_access_paths;
         "sanitize" >:: test_sanitize;
       ]
  |> Test.run
