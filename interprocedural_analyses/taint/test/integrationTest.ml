(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Interprocedural
open TestHelper

let test_integration context =
  TaintIntegrationTest.Files.dummy_dependency |> ignore;
  let test_paths =
    (* Shameful things happen here... *)
    Path.current_working_directory ()
    |> Path.show
    |> String.chop_suffix_exn ~suffix:"_build/default/interprocedural_analyses/taint/test"
    |> (fun root -> Path.create_absolute root)
    |> (fun root ->
         Path.create_relative ~root ~relative:"interprocedural_analyses/taint/test/integration/")
    |> fun root -> Path.list ~file_filter:(String.is_suffix ~suffix:".py") ~root ()
  in
  let run_test path =
    let check_expectation ~suffix actual =
      let output_filename ~suffix ~initial =
        if initial then
          Path.with_suffix path ~suffix
        else
          Path.with_suffix path ~suffix:(suffix ^ ".actual")
      in
      let write_output ~suffix ?(initial = false) content =
        try output_filename ~suffix ~initial |> File.create ~content |> File.write with
        | Unix.Unix_error _ ->
            failwith (Format.asprintf "Could not write `%s` file for %a" suffix Path.pp path)
      in
      let remove_old_output ~suffix =
        try output_filename ~suffix ~initial:false |> Path.show |> Sys.remove with
        | Sys_error _ ->
            (* be silent *)
            ()
      in
      let get_expected ~suffix =
        try Path.with_suffix path ~suffix |> File.create |> File.content with
        | Unix.Unix_error _ -> None
      in
      match get_expected ~suffix with
      | None ->
          (* expected file does not exist, create it *)
          write_output ~suffix actual ~initial:true
      | Some expected ->
          if String.equal expected actual then
            remove_old_output ~suffix
          else (
            write_output ~suffix actual;
            Printf.printf "Expectations differ for %s %s\n" suffix (Path.show path);
            assert_bool
              (Format.asprintf
                 "Expectations differ for %s %s\n%a"
                 suffix
                 (Path.show path)
                 (Test.diff ~print:String.pp)
                 (expected, actual))
              false )
    in
    let serialized_models =
      let source = File.create path |> File.content |> fun content -> Option.value_exn content in
      let model_source =
        try
          let model_path = Path.with_suffix path ~suffix:".pysa" in
          File.create model_path |> File.content
        with
        | Unix.Unix_error _ -> None
      in
      let handle = Path.show path |> String.split ~on:'/' |> List.last_exn in
      let check_call_graph_expectation call_graph =
        let dependencies = DependencyGraph.from_callgraph call_graph in
        let actual =
          Format.asprintf "@%s\nCall dependencies\n%a" "generated" DependencyGraph.pp dependencies
        in
        check_expectation ~suffix:".cg" actual
      in
      let check_overrides_expectation overrides =
        let actual =
          Format.asprintf "@%s\nOverrides\n%a" "generated" DependencyGraph.pp overrides
        in
        check_expectation ~suffix:".overrides" actual
      in
      let { callgraph; all_callables; environment; overrides } =
        initialize ~handle ?models:model_source ~context source
      in
      let dependencies =
        DependencyGraph.from_callgraph callgraph
        |> DependencyGraph.union overrides
        |> DependencyGraph.reverse
      in
      check_call_graph_expectation callgraph;
      check_overrides_expectation overrides;
      Analysis.compute_fixpoint
        ~configuration:(Configuration.Analysis.create ())
        ~scheduler:(Scheduler.mock ())
        ~environment
        ~analyses:[Taint.Analysis.abstract_kind]
        ~dependencies
        ~all_callables
        Fixpoint.Epoch.initial
      |> ignore;
      let serialized_model callable : string =
        let externalization =
          Interprocedural.Analysis.externalize ~environment Taint.Analysis.abstract_kind callable
          |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
          |> String.concat ~sep:""
        in
        externalization
      in
      List.map all_callables ~f:serialized_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:""
    in
    check_expectation ~suffix:".models" ("@" ^ "generated\n" ^ serialized_models)
  in
  assert_bool "No test paths to check." (not (List.is_empty test_paths));
  List.iter test_paths ~f:run_test


let () = "taint" >::: ["integration" >:: test_integration] |> TestHelper.run_with_taint_models
