(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2

let test_search_path _ =
  let assert_search_paths ?(search_paths = []) ~source_paths expected =
    let search_paths =
      List.map search_paths ~f:PyrePath.create_absolute
      |> List.map ~f:(fun root -> SearchPath.Root root)
    in
    let source_paths = List.map source_paths ~f:PyrePath.create_absolute in
    let to_search_path root = SearchPath.Root root in
    let search_paths =
      Configuration.Analysis.search_paths
        (Configuration.Analysis.create
           ~search_paths
           ~source_paths:(List.map source_paths ~f:to_search_path)
           ())
      |> List.map ~f:SearchPath.show
    in
    assert_equal ~printer:(List.to_string ~f:Fn.id) expected search_paths
  in
  assert_search_paths ~source_paths:["/a"] ["/a"];
  assert_search_paths ~source_paths:["/a"] ["/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"]
    ["/other"; "/another"; "/a"];
  assert_search_paths
    ~search_paths:["/other"; "/another"]
    ~source_paths:["/a"; "/b"]
    ["/other"; "/another"; "/a"; "/b"];
  ()


let test_extensions _ =
  let assert_extensions ~extensions expected =
    let extensions = List.map ~f:Configuration.Extension.create_extension extensions in
    assert_equal
      ~cmp:(List.equal [%compare.equal: Configuration.Extension.t])
      ~printer:(List.to_string ~f:Configuration.Extension.show)
      expected
      extensions
  in
  assert_extensions
    ~extensions:[".extension"]
    [{ Configuration.Extension.suffix = ".extension"; include_suffix_in_module_qualifier = false }];
  assert_extensions
    ~extensions:[".extension$include_suffix_in_module_qualifier"]
    [{ Configuration.Extension.suffix = ".extension"; include_suffix_in_module_qualifier = true }];
  ()


let test_change_indicator context =
  let open Configuration.ChangeIndicator in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "root": 42 } |};
  assert_not_parsed {| { "root": "/foo", "relative": [] } |};

  assert_parsed
    {| { "root": "/foo", "relative": "derp.txt" }|}
    ~expected:{ root = PyrePath.create_absolute "/foo"; relative = "derp.txt" };
  ()


let test_unwatched_files context =
  let open Configuration.UnwatchedFiles in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "root": 42 } |};
  assert_not_parsed {| { "root": "/foo", "checksum_path": [] } |};

  assert_parsed
    {| { "root": "/foo", "checksum_path": "derp.txt" }|}
    ~expected:{ root = PyrePath.create_absolute "/foo"; checksum_path = "derp.txt" };
  ()


let test_unwatched_dependency context =
  let open Configuration.UnwatchedDependency in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf "Unexpected parsing success: %a" Sexp.pp_hum (sexp_of_t actual)
        in
        assert_failure message
  in

  assert_not_parsed "{}";
  assert_not_parsed "42";
  assert_not_parsed {| { "change_indicator": 42 } |};
  assert_not_parsed {| { "files": [] } |};
  assert_not_parsed
    {| { "change_indicator": [], "files": { "root": "/foo", "checksum_path": "bar" } } |};
  assert_not_parsed {| { "change_indicator": { "root": "/foo", "relative": "bar" }, "files": [] } |};

  assert_parsed
    {|
       {
         "change_indicator": { "root": "/foo", "relative": "bar" },
         "files": { "root": "/baz", "checksum_path": "derp.txt" }
       }
    |}
    ~expected:
      {
        change_indicator =
          { Configuration.ChangeIndicator.root = PyrePath.create_absolute "/foo"; relative = "bar" };
        files =
          {
            Configuration.UnwatchedFiles.root = PyrePath.create_absolute "/baz";
            checksum_path = "derp.txt";
          };
      };
  ()


let test_scheduler_policies context =
  let open Configuration in
  let assert_parsed ~expected text =
    match Yojson.Safe.from_string text |> SchedulerPolicies.of_yojson with
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: SchedulerPolicies.t]
          ~printer:(fun wheel -> Sexp.to_string_hum ([%sexp_of: SchedulerPolicies.t] wheel))
          expected
          actual
    | Result.Error message -> assert_failure message
  in
  let assert_not_parsed text =
    match Yojson.Safe.from_string text |> SchedulerPolicies.of_yojson with
    | Result.Error _ -> ()
    | Result.Ok actual ->
        let message =
          Format.asprintf
            "Unexpected parsing success: %a"
            Sexp.pp_hum
            (SchedulerPolicies.sexp_of_t actual)
        in
        assert_failure message
  in
  assert_not_parsed "42";
  assert_not_parsed "[]";
  assert_not_parsed
    {|
    {
      "unknown_schedule_identifier": { "kind": "legacy_fixed_chunk_count" }
    }
    |};
  assert_not_parsed
    {|
    {
      "taint_fixpoint": { "kind": "unknown_schedule_policy" }
    }
    |};
  assert_not_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size"
      }
    }
    |};
  assert_not_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size",
        "minimum_chunk_size": "not_integer",
        "preferred_chunk_size": 1000
      }
    }
    |};
  assert_not_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size",
        "minimum_chunk_size": 1,
        "preferred_chunk_size": 1000
      }
    }
    |};
  assert_not_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size",
        "minimum_chunk_size": 1,
        "minimum_chunks_per_worker": 0,
        "preferred_chunk_size": 1000
      }
    }
    |};

  assert_parsed "{}" ~expected:SchedulerPolicies.empty;
  assert_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size",
        "minimum_chunk_size": 1,
        "minimum_chunks_per_worker": 100,
        "preferred_chunk_size": 1000
      }
    }
    |}
    ~expected:
      (SchedulerPolicies.of_alist_exn
         [
           ( ScheduleIdentifier.TaintFixpoint,
             SchedulerPolicy.FixedChunkSize
               {
                 minimum_chunk_size = Some 1;
                 minimum_chunks_per_worker = 100;
                 preferred_chunk_size = 1000;
               } );
         ]);
  assert_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_size",
        "minimum_chunks_per_worker": 100,
        "preferred_chunk_size": 1000
      }
    }
    |}
    ~expected:
      (SchedulerPolicies.of_alist_exn
         [
           ( ScheduleIdentifier.TaintFixpoint,
             SchedulerPolicy.FixedChunkSize
               {
                 minimum_chunk_size = None;
                 minimum_chunks_per_worker = 100;
                 preferred_chunk_size = 1000;
               } );
         ]);
  assert_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_count",
        "minimum_chunks_per_worker": 10,
        "minimum_chunk_size": 100,
        "preferred_chunks_per_worker": 1000
      }
    }
    |}
    ~expected:
      (SchedulerPolicies.of_alist_exn
         [
           ( ScheduleIdentifier.TaintFixpoint,
             SchedulerPolicy.FixedChunkCount
               {
                 minimum_chunks_per_worker = Some 10;
                 minimum_chunk_size = 100;
                 preferred_chunks_per_worker = 1000;
               } );
         ]);
  assert_parsed
    {|
    {
      "taint_fixpoint": {
        "kind": "fixed_chunk_count",
        "minimum_chunk_size": 1,
        "preferred_chunks_per_worker": 2
      },
      "call_graph": {
        "kind": "fixed_chunk_count",
        "minimum_chunk_size": 3,
        "preferred_chunks_per_worker": 4
      }
    }
    |}
    ~expected:
      (SchedulerPolicies.of_alist_exn
         [
           ( ScheduleIdentifier.TaintFixpoint,
             SchedulerPolicy.FixedChunkCount
               {
                 minimum_chunks_per_worker = None;
                 minimum_chunk_size = 1;
                 preferred_chunks_per_worker = 2;
               } );
           ( ScheduleIdentifier.CallGraph,
             SchedulerPolicy.FixedChunkCount
               {
                 minimum_chunks_per_worker = None;
                 minimum_chunk_size = 3;
                 preferred_chunks_per_worker = 4;
               } );
         ]);
  ()


let () =
  "configuration"
  >::: [
         "search_path" >:: test_search_path;
         "extensions" >:: test_extensions;
         "change_indicator" >:: test_change_indicator;
         "unwatched_files" >:: test_unwatched_files;
         "unwatched_dependency" >:: test_unwatched_dependency;
         "scheduler_policies" >:: test_scheduler_policies;
       ]
  |> Test.run
