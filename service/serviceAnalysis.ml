(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open Pyre


let overrides_of_source ~environment ~source =
  let open Annotated in
  let resolution = Environment.resolution environment () in
  let filter_overrides child_method =
    Method.overrides child_method ~resolution
    >>| fun ancestor_method -> (Method.name ancestor_method, Method.name child_method)
  in
  let record_overrides map (ancestor_method, child_method) =
    let update_children = function
      | Some children -> child_method :: children
      | None -> [child_method]
    in
    Statement.Access.Map.update map ancestor_method ~f:update_children
  in
  Preprocessing.classes source
  |> List.concat_map ~f:(Fn.compose Class.methods Class.create)
  |> List.filter_map ~f:filter_overrides
  |> List.fold ~init:Statement.Access.Map.empty ~f:record_overrides


let record_and_merge_call_graph ~environment ~call_graph ~path ~source =
  let record_and_merge_call_graph path map call_graph =
    CallGraphSharedMemory.add_callers ~path (Access.Map.keys call_graph);
    let add_call_graph ~key:caller ~data:callees =
      CallGraphSharedMemory.add_call_edges ~caller ~callees
    in
    Access.Map.iteri call_graph ~f:add_call_graph;
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  Analysis.CallGraph.create ~environment ~source
  |> record_and_merge_call_graph path call_graph


let record_overrides ~environment ~source =
  let record_overrides overrides_map =
    let record_override_edge ~key:ancestor ~data:children =
      CallGraphSharedMemory.add_overrides ~ancestor ~children
    in
    Access.Map.iteri overrides_map ~f:record_override_edge
  in
  overrides_of_source ~environment ~source
  |> record_overrides


let record_path_of_definitions ~path ~source =
  let defines = Preprocessing.defines source in
  let record_definition definition =
    let open Interprocedural.Callable in
    add_definition (make definition) path
  in
  List.iter ~f:record_definition defines;
  defines


let add_models ~model_source =
  let open Taint in
  let open Interprocedural in
  let add_model_to_memory Model.{ call_target; model } =
    Log.info "Adding taint model %S to shared memory" (Callable.target_name call_target);
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target
  in
  let models = Model.create ~model_source |> Or_error.ok_exn in
  List.iter models ~f:add_model_to_memory


let analyze ?taint_models_directory ~scheduler ~configuration ~environment ~handles:paths () =
  (* Add models *)
  let () =
    match taint_models_directory with
    | Some directory ->
        let directory = Path.create_absolute directory in
        let check_directory_exists directory =
          if not (Path.is_directory directory) then
            raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
        in
        check_directory_exists directory;
        Log.info "Finding taint models in %a" Path.pp directory;
        let add_models path =
          Path.create_absolute path
          |> File.create
          |> File.content
          >>| (fun model_source -> add_models ~model_source)
          |> ignore
        in
        let directory = Path.absolute directory in
        Sys.readdir directory
        |> Array.to_list
        |> List.map ~f:((^/) directory)
        |> List.iter ~f:add_models
    | None -> ()
  in

  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let record_overrides path =
    AstSharedMemory.get_source path
    >>| (fun source -> record_overrides ~environment ~source)
    |> ignore
  in
  List.iter paths ~f:record_overrides;
  Statistics.performance ~name:"Overrides recorded" ~timer ();

  Log.info "Building call graph...";
  let timer = Timer.start () in
  let call_graph =
    let build_call_graph map path =
      try
        AstSharedMemory.get_source path
        >>| (fun source -> record_and_merge_call_graph ~environment ~call_graph:map ~path ~source)
        |> Option.value ~default:map
      with TypeOrder.Untracked untracked_type ->
        Log.info "Error building call graph in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        map
    in
    ServiceScheduler.map_reduce
      scheduler
      ~configuration
      ~init:Access.Map.empty
      ~map:(fun _ paths -> List.fold paths ~init:Access.Map.empty ~f:build_call_graph)
      ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
      paths
  in
  Statistics.performance ~name:"Call graph built" ~timer ();
  Log.info "Call graph edges: %d" (Access.Map.length call_graph);

  let caller_map = CallGraph.reverse call_graph in

  let all_callables =
    let make_callables path =
      AstSharedMemory.get_source path
      >>| fun source ->
      record_path_of_definitions ~path ~source
      |> List.map ~f:Interprocedural.Callable.make
    in
    List.filter_map paths ~f:make_callables
    |> List.concat
  in

  let analyses = [Taint.Analysis.abstract_kind] in

  Log.info "Analysis fixpoint started...";
  let timer = Timer.start () in
  let iterations =
    Interprocedural.Analysis.compute_fixpoint
      ~workers:(ServiceScheduler.workers scheduler)
      ~analyses
      ~caller_map
      ~all_callables
      Interprocedural.Fixpoint.Epoch.initial
  in
  Statistics.performance ~name:"Analysis fixpoint complete" ~timer ();
  Log.info "Fixpoint iterations: %d" iterations
