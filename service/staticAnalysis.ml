(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Interprocedural
open Statement
open Pyre


let record_and_merge_call_graph ~environment ~call_graph ~path:_ ~source =
  let record_and_merge_call_graph map call_graph =
    Map.merge_skewed map call_graph ~combine:(fun ~key:_ left _ -> left)
  in
  DependencyGraph.create_callgraph ~environment ~source
  |> record_and_merge_call_graph call_graph


let record_overrides overrides =
  let record_override_edge ~key:member ~data:subtypes =
    DependencyGraphSharedMemory.add_overriding_types ~member ~subtypes
  in
  Access.Map.iteri overrides ~f:record_override_edge


let record_path_of_definitions ~path ~source =
  let defines = Preprocessing.defines ~include_stubs:true source in
  let record_classes { Node.value = class_node; _ } =
    (*
    Log.info "Recording class %a -> %a"
      Access.pp class_node.Class.name
      File.Handle.pp path;
    *)
    Callable.add_class_definition class_node.Class.name path
  in
  let record_toplevel_definition definition =
    let name = definition.Node.value.Define.name in
    match definition.Node.value.Define.parent with
    | None ->
        (*
        Log.info "Recording function %a -> %a"
          Access.pp name
          File.Handle.pp path;
        *)
        (* Only record top-level definitions. *)
        let () = Callable.add_function_definition name path in
        Callable.create_function name, definition
    | Some class_name ->
        if not (Callable.class_exists class_name) then
          begin
            Log.error "Method's class non-existing";
            Format.asprintf
              "Class %a for method %a not found"
              Access.pp class_name
              Access.pp name
            |> failwith
          end
        else
          Callable.create_method name, definition
  in
  List.iter ~f:record_classes (Preprocessing.classes source);
  List.map ~f:record_toplevel_definition defines


let add_models ~environment sources =
  let open Taint in
  let open Interprocedural in
  let add_model_to_memory ~key:call_target ~data:model =
    (*
    let () =
      Log.info
        "Adding taint model %S to shared memory\n%a\n"
        (Callable.external_target_name call_target)
        Result.pp_model_t model
    in
    *)
    Fixpoint.add_predefined Fixpoint.Epoch.initial call_target model
  in
  let models =
    let make_model Model.{ model; _ } =
      Result.empty_model |> Result.with_model Taint.Result.kind model
    in
    List.concat_map
      sources
      ~f:(fun model_source ->
          Model.create
            ~resolution:(TypeCheck.resolution environment ())
            ~model_source
            ()
          |> Or_error.ok_exn
        )
    |> List.map ~f:(fun model -> (model.Model.call_target, make_model model))
    |> Callable.Map.of_alist_reduce ~f:Analysis.join_models
  in
  Callable.Map.iteri models ~f:add_model_to_memory


let analyze
    ?taint_models_directory
    ~scheduler
    ~configuration:({
        Configuration.StaticAnalysis.configuration;
        dump_call_graph;
        _;
      } as analysis_configuration)
    ~environment
    ~handles:paths
    () =
  (* Add models *)
  let () =
    match taint_models_directory with
    | Some directory ->
        begin
          try
            let directory = Path.create_absolute directory in
            let check_directory_exists directory =
              if not (Path.is_directory directory) then
                raise
                  (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
            in
            check_directory_exists directory;
            Log.info "Finding taint models in %a" Path.pp directory;
            let get_source_models path =
              Path.create_absolute path
              |> File.create
              |> File.content
            in
            let directory = Path.absolute directory in
            Sys.readdir directory
            |> Array.to_list
            |> List.filter ~f:(String.is_suffix ~suffix:".pysa")
            |> List.map ~f:((^/) directory)
            |> List.filter_map ~f:get_source_models
            |> add_models ~environment
          with exn ->
            Log.dump
              "Error getting taint models: %s" (Exn.to_string exn);
            raise exn
        end
    | None -> ()
  in

  Log.info "Recording overrides...";
  let timer = Timer.start () in
  let overrides =
    let combine ~key:_ left right =
      List.rev_append left right
    in
    let build_overrides overrides path =
      try
        match Ast.SharedMemory.Sources.get path with
        | None -> overrides
        | Some source ->
            let new_overrides = DependencyGraph.create_overrides ~environment ~source in
            record_overrides new_overrides;
            Map.merge_skewed overrides new_overrides ~combine
      with TypeOrder.Untracked untracked_type ->
        Log.info
          "Error building overrides in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        overrides
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:DependencyGraph.empty_overrides
      ~map:(fun _ paths -> List.fold paths ~init:DependencyGraph.empty_overrides ~f:build_overrides)
      ~reduce:(Map.merge_skewed ~combine)
      ~inputs:paths
      ()
  in
  Statistics.performance ~name:"Overrides recorded" ~timer ();

  Log.info "Building call graph...";
  let timer = Timer.start () in
  let callgraph =
    let build_call_graph call_graph path =
      try
        Ast.SharedMemory.Sources.get path
        >>| (fun source -> record_and_merge_call_graph ~environment ~call_graph ~path ~source)
        |> Option.value ~default:call_graph
      with TypeOrder.Untracked untracked_type ->
        Log.info "Error building call graph in path %a for untracked type %a"
          File.Handle.pp path
          Type.pp untracked_type;
        call_graph
    in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:Callable.RealMap.empty
      ~map:(fun _ paths -> List.fold paths ~init:Callable.RealMap.empty ~f:build_call_graph)
      ~reduce:(Map.merge_skewed ~combine:(fun ~key:_ left _ -> left))
      ~inputs:paths
      ()
  in
  Statistics.performance ~name:"Call graph built" ~timer ();

  Log.info "Call graph edges: %d" (Callable.RealMap.length callgraph);
  if dump_call_graph then
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.dump ~configuration;

  let callables, stubs =
    let classify_source (callables, stubs) (callable, define) =
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
  let real_callables =
    let model_definitions = ref 0 in
    let record_initial_inferred_model callable =
      let open Interprocedural in
      if Fixpoint.get_meta_data callable <> None then
        (Int.incr model_definitions; Some callable)
      else
        let () = Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model in
        Some callable
    in
    let result = List.filter_map callables ~f:record_initial_inferred_model in
    let () =
      Log.info
        "Found %d source definitions with predefined models."
        !model_definitions
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
  let override_dependencies = DependencyGraph.from_overrides overrides in
  let dependencies =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union override_dependencies
    |> DependencyGraph.reverse
  in
  let override_targets = (Callable.Map.keys override_dependencies :> Callable.t list) in
  let () =
    let add_predefined callable =
      Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model
    in
    List.iter override_targets ~f:add_predefined
  in
  let all_callables = List.rev_append override_targets real_callables in
  Log.info
    "Analysis fixpoint started for %d overrides %d functions..."
    (List.length override_targets)
    (List.length real_callables);
  let timer = Timer.start () in
  let () =
    try
      let iterations =
        Interprocedural.Analysis.compute_fixpoint
          ~configuration
          ~scheduler
          ~environment
          ~analyses
          ~dependencies
          ~all_callables
          Interprocedural.Fixpoint.Epoch.initial
      in
      Log.info "Fixpoint iterations: %d" iterations;
    with
      exn ->
        Interprocedural.Analysis.save_results
          ~configuration:analysis_configuration
          ~analyses
          all_callables;
        raise exn
  in
  let () =
    Interprocedural.Analysis.save_results
      ~configuration:analysis_configuration
      ~analyses
      all_callables
  in
  let errors = Interprocedural.Analysis.extract_errors scheduler ~configuration all_callables in
  Statistics.performance ~name:"Analysis fixpoint complete" ~timer ();
  errors
