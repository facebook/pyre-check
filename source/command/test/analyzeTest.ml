(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.Analyze

let test_json_parsing context =
  let assert_parsed ~expected json =
    match AnalyzeConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: AnalyzeConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: AnalyzeConfiguration.t] result))
          expected
          actual
  in

  let dummy_analyze_configuration =
    {
      AnalyzeConfiguration.base = BaseConfigurationTest.dummy_base_configuration;
      pyrefly_results = None;
      dump_call_graph = None;
      dump_model_query_results = None;
      find_missing_flows = None;
      infer_self_tito = true;
      infer_argument_tito = false;
      maximum_model_source_tree_width = None;
      maximum_model_sink_tree_width = None;
      maximum_model_tito_tree_width = None;
      maximum_tree_depth_after_widening = None;
      maximum_return_access_path_width = None;
      maximum_return_access_path_depth_after_widening = None;
      maximum_tito_collapse_depth = None;
      maximum_tito_positions = None;
      maximum_overrides_to_analyze = None;
      maximum_trace_length = None;
      maximum_tito_depth = None;
      repository_root = None;
      rule_filter = None;
      source_filter = None;
      sink_filter = None;
      transform_filter = None;
      save_results_to = None;
      output_format = Configuration.TaintOutputFormat.Json;
      strict = false;
      taint_model_paths = [];
      use_cache = false;
      build_cache_only = false;
      no_verify = false;
      verify_dsl = false;
      verify_taint_config_only = false;
      check_invariants = false;
      limit_entrypoints = false;
      compact_ocaml_heap = false;
      saved_state = Configuration.StaticAnalysis.SavedState.empty;
      compute_coverage = false;
      scheduler_policies = Configuration.SchedulerPolicies.empty;
      higher_order_call_graph_max_iterations = None;
      maximum_target_depth = None;
      maximum_parameterized_targets_at_call_site = None;
    }
  in

  assert_parsed
    (`Assoc (("dump_call_graph", `String "/call-graph") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        dump_call_graph = Some (PyrePath.create_absolute "/call-graph");
      };
  assert_parsed
    (`Assoc
      (("dump_model_query_results", `String "/model-query") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        dump_model_query_results = Some (PyrePath.create_absolute "/model-query");
      };
  assert_parsed
    (`Assoc (("find_missing_flows", `String "obscure") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        find_missing_flows = Some Configuration.MissingFlowKind.Obscure;
      };
  assert_parsed
    (`Assoc (("infer_self_tito", `Bool false) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with infer_self_tito = false };
  assert_parsed
    (`Assoc (("infer_argument_tito", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with infer_argument_tito = true };
  assert_parsed
    (`Assoc (("maximum_tito_depth", `Int 5) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_tito_depth = Some 5 };
  assert_parsed
    (`Assoc (("maximum_trace_length", `Int 5) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_trace_length = Some 5 };
  assert_parsed
    (`Assoc (("maximum_model_source_tree_width", `Int 30) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_model_source_tree_width = Some 30 };
  assert_parsed
    (`Assoc (("maximum_model_sink_tree_width", `Int 30) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_model_sink_tree_width = Some 30 };
  assert_parsed
    (`Assoc (("maximum_model_tito_tree_width", `Int 30) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_model_tito_tree_width = Some 30 };
  assert_parsed
    (`Assoc
      (("maximum_tree_depth_after_widening", `Int 5) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_tree_depth_after_widening = Some 5 };
  assert_parsed
    (`Assoc (("maximum_return_access_path_width", `Int 5) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_return_access_path_width = Some 5 };
  assert_parsed
    (`Assoc
      (("maximum_return_access_path_depth_after_widening", `Int 5)
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with maximum_return_access_path_depth_after_widening = Some 5 };
  assert_parsed
    (`Assoc (("maximum_tito_collapse_depth", `Int 6) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_tito_collapse_depth = Some 6 };
  assert_parsed
    (`Assoc (("maximum_tito_positions", `Int 50) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_tito_positions = Some 50 };
  assert_parsed
    (`Assoc (("no_verify", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with no_verify = true };
  assert_parsed
    (`Assoc (("verify_dsl", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with verify_dsl = true };
  assert_parsed
    (`Assoc (("repository_root", `String "/root") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with repository_root = Some (PyrePath.create_absolute "/root") };
  assert_parsed
    (`Assoc (("rule_filter", `List [`Int 1; `Int 2]) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with rule_filter = Some [1; 2] };
  assert_parsed
    (`Assoc
      (("source_filter", `List [`String "UserControlled"; `String "Header"])
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with source_filter = Some ["UserControlled"; "Header"] };
  assert_parsed
    (`Assoc
      (("sink_filter", `List [`String "SQL"; `String "RCE"])
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with sink_filter = Some ["SQL"; "RCE"] };
  assert_parsed
    (`Assoc
      (("transform_filter", `List [`String "FileSystem"; `String "MyTransform"])
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with transform_filter = Some ["FileSystem"; "MyTransform"] };
  assert_parsed
    (`Assoc (("save_results_to", `String "/result") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        save_results_to = Some (PyrePath.create_absolute "/result");
      };
  assert_parsed
    (`Assoc (("output_format", `String "json") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with output_format = Configuration.TaintOutputFormat.Json };
  assert_parsed
    (`Assoc (("output_format", `String "sharded-json") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        output_format = Configuration.TaintOutputFormat.ShardedJson;
      };
  assert_parsed
    (`Assoc (("strict", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with strict = true };
  assert_parsed
    (`Assoc
      (("taint_model_paths", `List [`String "/taint"; `String "/model"])
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        taint_model_paths = [PyrePath.create_absolute "/taint"; PyrePath.create_absolute "/model"];
      };
  assert_parsed
    (`Assoc (("use_cache", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with use_cache = true };
  assert_parsed
    (`Assoc (("build_cache_only", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with build_cache_only = true };
  assert_parsed
    (`Assoc (("check_invariants", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with check_invariants = true };
  assert_parsed
    (`Assoc
      (( "saved_state",
         `Assoc
           [
             "watchman_root", `String "/root";
             "project_name", `String "my_project";
             "preset", `String "some_preset";
             "cache_critical_files", `List [`String "*.py"; `String "*.pysa"];
           ] )
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        saved_state =
          {
            watchman_root = Some "/root";
            project_name = Some "my_project";
            preset = Some "some_preset";
            cache_critical_files = ["*.py"; "*.pysa"];
          };
      };
  assert_parsed
    (`Assoc (("compute_coverage", `Bool true) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with compute_coverage = true };
  assert_parsed
    (`Assoc
      (("higher_order_call_graph_max_iterations", `Int 101) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with higher_order_call_graph_max_iterations = Some 101 };
  assert_parsed
    (`Assoc (("maximum_target_depth", `Int 412) :: BaseConfigurationTest.dummy_base_json))
    ~expected:{ dummy_analyze_configuration with maximum_target_depth = Some 412 };
  assert_parsed
    (`Assoc
      (("maximum_parameterized_targets_at_call_site", `Int 410)
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with maximum_parameterized_targets_at_call_site = Some 410 };
  assert_parsed
    (`Assoc
      (( "scheduler_policies",
         `Assoc
           [
             ( "taint_fixpoint",
               `Assoc
                 [
                   "kind", `String "fixed_chunk_size";
                   "minimum_chunks_per_worker", `Int 10;
                   "preferred_chunk_size", `Int 100;
                 ] );
           ] )
      :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      {
        dummy_analyze_configuration with
        scheduler_policies =
          Configuration.SchedulerPolicies.of_alist_exn
            [
              ( Configuration.ScheduleIdentifier.TaintFixpoint,
                Configuration.SchedulerPolicy.FixedChunkSize
                  {
                    minimum_chunk_size = None;
                    minimum_chunks_per_worker = 10;
                    preferred_chunk_size = 100;
                  } );
            ];
      };
  assert_parsed
    (`Assoc (("pyrefly_results", `String "/foo") :: BaseConfigurationTest.dummy_base_json))
    ~expected:
      { dummy_analyze_configuration with pyrefly_results = Some (PyrePath.create_absolute "/foo") };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
