(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Interprocedural
open Statement
open Pyre


let overrides_of_source ~environment ~source =
  let open Annotated in
  let resolution = TypeCheck.resolution environment () in
  let filter_overrides child_method =
    Method.parent child_method
    |> Resolution.class_definition resolution
    >>| Annotated.Class.create
    >>= (fun definition ->
        Class.overrides
          definition
          ~name:(Statement.Define.unqualified_name (Method.define child_method))
          ~resolution)
    >>| fun ancestor ->
    let ancestor_parent =
      Attribute.parent ancestor
      |> Type.show
      |> Expression.Access.create
    in
    (ancestor_parent @ Attribute.access ancestor, Method.name child_method)
  in
  let record_overrides map (ancestor_method, child_method) =
    let ancestor_callable = Interprocedural.Callable.create_real ancestor_method in
    let child_callable = Interprocedural.Callable.create_real child_method in
    let update_children = function
      | Some children -> child_callable :: children
      | None -> [child_callable]
    in
    Interprocedural.Callable.Map.update map ancestor_callable ~f:update_children
  in
  Preprocessing.classes source
  |> List.concat_map ~f:(Fn.compose (Class.methods ~resolution) Class.create)
  |> List.filter_map ~f:filter_overrides
  |> List.fold ~init:Interprocedural.Callable.Map.empty ~f:record_overrides


let record_and_merge_call_graph ~environment ~call_graph ~path ~source =
  let record_and_merge_call_graph path map call_graph =
    DependencyGraphSharedMemory.add_callers ~path (Callable.Map.keys call_graph);
    let add_call_graph ~key:caller ~data:callees =
      DependencyGraphSharedMemory.add_call_edges ~caller ~callees
    in
    Callable.Map.iteri call_graph ~f:add_call_graph;
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  DependencyGraph.create ~environment ~source
  |> record_and_merge_call_graph path call_graph


let record_overrides ~environment ~source =
  let record_overrides overrides_map =
    let record_override_edge ~key:ancestor ~data:children =
      DependencyGraphSharedMemory.add_overrides ~ancestor ~children
    in
    Callable.Map.iteri overrides_map ~f:record_override_edge
  in
  overrides_of_source ~environment ~source
  |> record_overrides


let record_path_of_definitions ~path ~source =
  let defines = Preprocessing.defines ~include_stubs:true source in
  let record_definition definition =
    let open Interprocedural.Callable in
    add_definition (create definition) path
  in
  List.iter ~f:record_definition defines;
  defines


let add_models ~environment ~model_source =
  let open Taint in
  let open Interprocedural in
  let add_model_to_memory Model.{ call_target; model; _ } =
    Log.info "Adding taint model %S to shared memory" (Callable.external_target_name call_target);
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined Fixpoint.Epoch.predefined call_target
  in
  let models =
    Model.create
      ~resolution:(TypeCheck.resolution environment ())
      ~model_source
      ()
    |> Or_error.ok_exn
  in
  List.iter models ~f:add_model_to_memory


let analyze
    ?taint_models_directory
    ~scheduler
    ~configuration:({ Configuration.StaticAnalysis.configuration; _ } as analysis_configuration)
    ~environment
    ~handles:paths
    () =
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
          >>| (fun model_source -> add_models ~environment ~model_source)
          |> ignore
        in
        let directory = Path.absolute directory in
        Sys.readdir directory
        |> Array.to_list
        |> List.filter ~f:(String.is_suffix ~suffix:".pysa")
        |> List.map ~f:((^/) directory)
        |> List.iter ~f:add_models
    | None -> ()
  in

  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let record_overrides path =
    Ast.SharedMemory.Sources.get path
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
        Ast.SharedMemory.Sources.get path
        >>| (fun source -> record_and_merge_call_graph ~environment ~call_graph:map ~path ~source)
        |> Option.value ~default:map
      with TypeOrder.Untracked untracked_type ->
        Log.info "Error building call graph in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        map
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:Callable.Map.empty
      ~map:(fun _ paths -> List.fold paths ~init:Callable.Map.empty ~f:build_call_graph)
      ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
      ~inputs:paths
      ()
  in
  Statistics.performance ~name:"Call graph built" ~timer ();
  Log.info "Call graph edges: %d" (Callable.Map.length call_graph);

  let caller_map = DependencyGraph.reverse call_graph in

  let callables, stubs =
    let classify_source (callables, stubs) define =
      let callable = Interprocedural.Callable.create define in
      if Define.is_stub define.Node.value then
        callables, callable :: stubs
      else
        callable :: callables, stubs
    in
    let make_callables result path =
      Ast.SharedMemory.Sources.get path
      >>|
      (fun source ->
         record_path_of_definitions ~path ~source
         |> List.fold ~f:classify_source ~init:result)
      |> Option.value ~default:result
    in
    List.fold paths ~f:make_callables ~init:([], [])
  in
  let all_callables =
    let skipped_definitions = ref 0 in
    let record_initial_inferred_model callable =
      let open Interprocedural in
      if Fixpoint.get_meta_data callable <> None then
        (Int.incr skipped_definitions; None)
      else
        let () = Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model in
        Some callable
    in
    let result = List.filter_map callables ~f:record_initial_inferred_model in
    let () =
      Log.info
        "Skipping %d source definitions due to initial models or duplicates."
        !skipped_definitions
    in
    result
  in
  let () =
    let obscure_stubs = ref 0 in
    let record_obscure_stub_model callable =
      let open Interprocedural in
      if Fixpoint.get_meta_data callable = None then begin
        Int.incr obscure_stubs;
        Fixpoint.add_predefined Fixpoint.Epoch.predefined callable Result.obscure_model
      end
    in
    List.iter stubs ~f:record_obscure_stub_model;
    Log.info
      "Added %d obscure models for stubs without source definition."
      !obscure_stubs
  in
  let analyses = [Taint.Analysis.abstract_kind] in

  Log.info "Analysis fixpoint started...";
  let timer = Timer.start () in
  let iterations =
    Interprocedural.Analysis.compute_fixpoint
      ~configuration
      ~scheduler
      ~environment
      ~analyses
      ~caller_map
      ~all_callables
      Interprocedural.Fixpoint.Epoch.initial
  in
  Log.info "Fixpoint iterations: %d" iterations;
  let () =
    Interprocedural.Analysis.save_results ~configuration:analysis_configuration all_callables
  in
  let errors = Interprocedural.Analysis.extract_errors scheduler ~configuration all_callables in
  Statistics.performance ~name:"Analysis fixpoint complete" ~timer ();
  errors
