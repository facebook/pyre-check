(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open Interprocedural
open TestHelper

type mismatch_file = {
  path: PyrePath.t;
  suffix: string;
  expected: string;
  actual: string;
}

let test_integration path context =
  TaintIntegrationTest.Files.dummy_dependency |> ignore;
  let create_expected_and_actual_files ~suffix actual =
    let output_filename ~suffix ~initial =
      if initial then
        PyrePath.with_suffix path ~suffix
      else
        PyrePath.with_suffix path ~suffix:(suffix ^ ".actual")
    in
    let write_output ~suffix ?(initial = false) content =
      try output_filename ~suffix ~initial |> File.create ~content |> File.write with
      | Unix.Unix_error _ ->
          failwith (Format.asprintf "Could not write `%s` file for %a" suffix PyrePath.pp path)
    in
    let remove_old_output ~suffix =
      try output_filename ~suffix ~initial:false |> PyrePath.show |> Sys.remove with
      | Sys_error _ ->
          (* be silent *)
          ()
    in
    let get_expected ~suffix =
      try PyrePath.with_suffix path ~suffix |> File.create |> File.content with
      | Unix.Unix_error _ -> None
    in
    match get_expected ~suffix with
    | None ->
        (* expected file does not exist, create it *)
        write_output ~suffix actual ~initial:true;
        None
    | Some expected ->
        if String.equal expected actual then (
          remove_old_output ~suffix;
          None)
        else (
          write_output ~suffix actual;
          Some { path; suffix; expected; actual })
  in
  let error_on_actual_files { path; suffix; expected; actual } =
    Printf.printf
      "%s"
      (Format.asprintf
         "Expectations differ for %s %s\n%a"
         suffix
         (PyrePath.show path)
         (Test.diff ~print:String.pp)
         (expected, actual))
  in
  let divergent_files, serialized_models =
    let source = File.create path |> File.content |> fun content -> Option.value_exn content in
    let models_source =
      try
        let model_path = PyrePath.with_suffix path ~suffix:".pysa" in
        File.create model_path |> File.content
      with
      | Unix.Unix_error _ -> None
    in
    let taint_configuration =
      try
        let path = PyrePath.with_suffix path ~suffix:".config" in
        File.create path
        |> File.content
        |> Option.map ~f:(fun content ->
               Taint.TaintConfiguration.parse [path, Yojson.Safe.from_string content]
               |> Taint.TaintConfiguration.exception_on_error)
      with
      | Unix.Unix_error _ -> None
    in
    let add_initial_models = Option.is_none models_source && Option.is_none taint_configuration in
    let taint_configuration =
      taint_configuration |> Option.value ~default:Taint.TaintConfiguration.default
    in
    let handle = PyrePath.show path |> String.split ~on:'/' |> List.last_exn in
    let create_call_graph_files call_graph =
      let actual =
        Format.asprintf
          "@%s\nCall dependencies\n%a"
          "generated"
          TargetGraph.pp
          (CallGraph.WholeProgramCallGraph.to_target_graph call_graph)
      in
      create_expected_and_actual_files ~suffix:".cg" actual
    in
    let create_overrides_files overrides =
      let actual =
        Format.asprintf
          "@%s\nOverrides\n%a"
          "generated"
          TargetGraph.pp
          (DependencyGraph.Reversed.to_target_graph
             (DependencyGraph.Reversed.from_overrides overrides))
      in
      create_expected_and_actual_files ~suffix:".overrides" actual
    in
    let {
      whole_program_call_graph;
      define_call_graphs;
      environment;
      override_graph_heap;
      override_graph_shared_memory;
      initial_models;
      initial_callables;
      stubs;
      class_interval_graph;
      _;
    }
      =
      initialize ~handle ?models_source ~add_initial_models ~taint_configuration ~context source
    in
    let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
      DependencyGraph.build_whole_program_dependency_graph
        ~prune:true
        ~initial_callables
        ~call_graph:whole_program_call_graph
        ~overrides:override_graph_heap
    in
    let fixpoint_state =
      Fixpoint.compute
        ~scheduler:(Test.mock_scheduler ())
        ~type_environment:environment
        ~override_graph:override_graph_shared_memory
        ~dependency_graph
        ~context:
          {
            Fixpoint.Context.type_environment = environment;
            class_interval_graph;
            define_call_graphs;
          }
        ~initial_callables:(FetchCallables.get_callables initial_callables)
        ~stubs
        ~override_targets
        ~callables_to_analyze
        ~initial_models
        ~max_iterations:100
        ~epoch:Fixpoint.Epoch.initial
    in
    let serialize_model callable : string =
      let externalization =
        let filename_lookup =
          Analysis.TypeEnvironment.ReadOnly.ast_environment environment
          |> Analysis.AstEnvironment.ReadOnly.get_relative
        in
        Taint.Reporting.fetch_and_externalize
          ~fixpoint_state
          ~filename_lookup
          ~override_graph:override_graph_shared_memory
          callable
        |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
        |> String.concat ~sep:""
      in
      externalization
    in

    let divergent_files =
      [create_call_graph_files whole_program_call_graph; create_overrides_files override_graph_heap]
    in
    let serialized_models =
      List.rev_append (Registry.targets initial_models) callables_to_analyze
      |> Target.Set.of_list
      |> Target.Set.elements
      |> List.map ~f:serialize_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:""
    in
    let () = Fixpoint.cleanup fixpoint_state in
    let () = OverrideGraph.SharedMemory.cleanup override_graph_shared_memory override_graph_heap in
    divergent_files, serialized_models
  in
  let divergent_files =
    create_expected_and_actual_files ~suffix:".models" ("@" ^ "generated\n" ^ serialized_models)
    :: divergent_files
    |> List.filter_opt
  in
  List.iter divergent_files ~f:error_on_actual_files;
  if not (List.is_empty divergent_files) then
    let message =
      List.map divergent_files ~f:(fun { path; suffix; _ } ->
          Format.asprintf "%a%s" PyrePath.pp path suffix)
      |> String.concat ~sep:", "
      |> Format.sprintf "Found differences in %s."
    in
    assert_bool message false


let test_paths =
  let file_filter name =
    String.is_suffix ~suffix:".py" name
    && (not (String.contains name '#'))
    && not (String.contains name '~')
  in
  PyrePath.current_working_directory ()
  |> (fun path ->
       PyrePath.search_upwards ~target:"source" ~target_type:PyrePath.FileType.Directory ~root:path
       |> Option.value ~default:path)
  |> (fun root ->
       PyrePath.create_relative
         ~root
         ~relative:"source/interprocedural_analyses/taint/test/integration/")
  |> fun root -> PyrePath.list ~file_filter ~root ()


let test_paths_found _ =
  if List.is_empty test_paths then
    assert_bool "No test paths to check." false


let () =
  test_paths
  |> List.map ~f:(fun path -> PyrePath.last path >:: test_integration path)
  |> List.cons ("paths_found" >:: test_paths_found)
  |> (fun tests -> "taint" >::: tests)
  |> Test.run
