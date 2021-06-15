(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre
open Analysis
open Interprocedural
open TestHelper

type mismatch_file = {
  path: Path.t;
  suffix: string;
  expected: string;
  actual: string;
}

let test_integration path context =
  TaintIntegrationTest.Files.dummy_dependency |> ignore;
  let create_expected_and_actual_files ~suffix actual =
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
        write_output ~suffix actual ~initial:true;
        None
    | Some expected ->
        if String.equal expected actual then (
          remove_old_output ~suffix;
          None )
        else (
          write_output ~suffix actual;
          Some { path; suffix; expected; actual } )
  in
  let error_on_actual_files { path; suffix; expected; actual } =
    Printf.printf
      "%s"
      (Format.asprintf
         "Expectations differ for %s %s\n%a"
         suffix
         (Path.show path)
         (Test.diff ~print:String.pp)
         (expected, actual))
  in
  let divergent_files, serialized_models =
    let source = File.create path |> File.content |> fun content -> Option.value_exn content in
    let model_source =
      try
        let model_path = Path.with_suffix path ~suffix:".pysa" in
        File.create model_path |> File.content
      with
      | Unix.Unix_error _ -> None
    in
    let taint_configuration =
      try
        let path = Path.with_suffix path ~suffix:".config" in
        File.create path
        |> File.content
        |> Option.map ~f:(fun content ->
               Taint.TaintConfiguration.parse [Yojson.Safe.from_string content])
        |> Option.value ~default:Taint.TaintConfiguration.default
      with
      | Unix.Unix_error _ -> Taint.TaintConfiguration.default
    in
    let handle = Path.show path |> String.split ~on:'/' |> List.last_exn in
    let create_call_graph_files call_graph =
      let dependencies = DependencyGraph.from_callgraph call_graph in
      let actual =
        Format.asprintf "@%s\nCall dependencies\n%a" "generated" DependencyGraph.pp dependencies
      in
      create_expected_and_actual_files ~suffix:".cg" actual
    in
    let create_overrides_files overrides =
      let actual = Format.asprintf "@%s\nOverrides\n%a" "generated" DependencyGraph.pp overrides in
      create_expected_and_actual_files ~suffix:".overrides" actual
    in
    let { callgraph; callables_to_analyze; initial_models_callables; environment; overrides } =
      initialize ~handle ?models:model_source ~taint_configuration ~context source
    in
    let dependencies =
      DependencyGraph.from_callgraph callgraph
      |> DependencyGraph.union overrides
      |> DependencyGraph.reverse
    in
    Analysis.compute_fixpoint
      ~scheduler:(Test.mock_scheduler ())
      ~environment
      ~analysis:TaintAnalysis.abstract_kind
      ~dependencies
      ~filtered_callables:Callable.Set.empty
      ~all_callables:callables_to_analyze
      Fixpoint.Epoch.initial
    |> ignore;
    let serialized_model callable : string =
      let externalization =
        let filename_lookup =
          TypeEnvironment.ReadOnly.ast_environment environment
          |> AstEnvironment.ReadOnly.get_relative
        in
        Taint.Reporting.fetch_and_externalize ~filename_lookup callable
        |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
        |> String.concat ~sep:""
      in
      externalization
    in

    let divergent_files = [create_call_graph_files callgraph; create_overrides_files overrides] in
    ( divergent_files,
      List.rev_append initial_models_callables callables_to_analyze
      |> Callable.Set.of_list
      |> Callable.Set.elements
      |> List.map ~f:serialized_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:"" )
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
          Format.asprintf "%a%s" Path.pp path suffix)
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
  Path.current_working_directory ()
  |> (fun path ->
       Path.search_upwards ~target:"source" ~target_type:Path.FileType.Directory ~root:path
       |> Option.value ~default:path)
  |> (fun root ->
       Path.create_relative
         ~root
         ~relative:"source/interprocedural_analyses/taint/test/integration/")
  |> fun root -> Path.list ~file_filter ~root ()


let test_paths_found _ =
  if List.is_empty test_paths then
    assert_bool "No test paths to check." false


let () =
  test_paths
  |> List.map ~f:(fun path -> Path.last path, test_integration path)
  |> List.cons ("paths_found", test_paths_found)
  |> TestHelper.run_with_taint_models ~name:"taint"
