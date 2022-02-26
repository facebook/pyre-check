(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Taint
module Target = Interprocedural.Target

(* Registers the Taint analysis with the interprocedural analysis framework. *)
include Taint.Result.Register (struct
  include Taint.Result

  let initialize_configuration
      ~static_analysis_configuration:
        { Configuration.StaticAnalysis.configuration = { taint_model_paths; _ }; _ }
    =
    (* In order to save time, sanity check models before starting the analysis. *)
    Log.info "Verifying model syntax and configuration.";
    let timer = Timer.start () in
    ModelParser.get_model_sources ~paths:taint_model_paths
    |> List.iter ~f:(fun (path, source) -> ModelParser.verify_model_syntax ~path ~source);
    let (_ : TaintConfiguration.t) =
      TaintConfiguration.create
        ~rule_filter:None
        ~find_missing_flows:None
        ~dump_model_query_results_path:None
        ~maximum_trace_length:None
        ~maximum_tito_depth:None
        ~taint_model_paths
      |> TaintConfiguration.abort_on_error
    in
    Statistics.performance
      ~name:"Verified model syntax and configuration"
      ~phase_name:"Verifying model syntax and configuration"
      ~timer
      ()


  type model_query_data = {
    queries: ModelParser.Internal.ModelQuery.rule list;
    taint_configuration: TaintConfiguration.t;
  }

  type parse_sources_result = {
    initialize_result: Model.t Interprocedural.AnalysisResult.initialize_result;
    query_data: model_query_data option;
  }

  let generate_models_from_queries
      ~scheduler
      ~static_analysis_configuration:
        { Configuration.StaticAnalysis.rule_filter; find_missing_flows; _ }
      ~environment
      ~callables
      ~stubs
      ~initialize_result:{ Interprocedural.AnalysisResult.initial_models = models; skip_overrides }
      { queries; taint_configuration }
    =
    let resolution =
      Analysis.TypeCheck.resolution
        (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module Analysis.TypeCheck.DummyContext)
    in
    let models =
      let callables =
        Hash_set.fold stubs ~f:(Core.Fn.flip List.cons) ~init:callables
        |> List.filter_map ~f:(function
               | `Function _ as callable -> Some (callable :> Target.callable_t)
               | `Method _ as callable -> Some (callable :> Target.callable_t)
               | _ -> None)
      in
      TaintModelQuery.ModelQuery.apply_all_rules
        ~resolution
        ~scheduler
        ~configuration:taint_configuration
        ~rule_filter
        ~rules:queries
        ~callables
        ~stubs
        ~environment
        ~models
    in
    let remove_sinks models = Target.Map.map ~f:Model.remove_sinks models in
    let add_obscure_sinks models =
      let add_obscure_sink models callable =
        let model =
          Target.Map.find models callable
          |> Option.value ~default:Model.empty_model
          |> Model.add_obscure_sink ~resolution ~call_target:callable
          |> Model.remove_obscureness
        in
        Target.Map.set models ~key:callable ~data:model
      in
      stubs
      |> Hash_set.filter ~f:(fun callable ->
             Target.Map.find models callable >>| Model.is_obscure |> Option.value ~default:true)
      |> Hash_set.fold ~f:add_obscure_sink ~init:models
    in
    let find_missing_flows =
      find_missing_flows >>= TaintConfiguration.missing_flows_kind_from_string
    in
    let models =
      match find_missing_flows with
      | Some Obscure -> models |> remove_sinks |> add_obscure_sinks
      | Some Type -> models |> remove_sinks
      | None -> models
    in
    { Interprocedural.AnalysisResult.initial_models = models; skip_overrides }


  let parse_models_and_queries_from_sources
      ~scheduler
      ~static_analysis_configuration:
        {
          Configuration.StaticAnalysis.verify_models;
          configuration = { taint_model_paths; _ };
          rule_filter;
          find_missing_flows;
          dump_model_query_results;
          maximum_trace_length;
          maximum_tito_depth;
          _;
        }
      ~environment
      ~callables
      ~stubs
    =
    let resolution =
      Analysis.TypeCheck.resolution
        (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module Analysis.TypeCheck.DummyContext)
    in
    let create_models ~taint_configuration sources =
      let map state sources =
        List.fold
          sources
          ~init:state
          ~f:(fun (accumulated_models, errors, skip_overrides, queries) (path, source) ->
            let {
              ModelParser.models;
              errors = new_errors;
              skip_overrides = new_skip_overrides;
              queries = new_queries;
            }
              =
              ModelParser.parse
                ~resolution
                ~path
                ~source
                ~configuration:taint_configuration
                ~callables
                ~stubs
                ?rule_filter
                ()
            in
            let merged_models =
              Target.Map.merge accumulated_models models ~f:(fun ~key:_ -> function
                | `Both (left, right) -> Some (Model.join left right)
                | `Left model
                | `Right model ->
                    Some model)
            in
            ( merged_models,
              List.rev_append new_errors errors,
              Set.union skip_overrides new_skip_overrides,
              List.rev_append new_queries queries ))
      in
      let reduce
          (models_left, errors_left, skip_overrides_left, queries_left)
          (models_right, errors_right, skip_overrides_right, queries_right)
        =
        let merge_models ~key:_ = function
          | `Left model
          | `Right model ->
              Some model
          | `Both (left, right) -> Some (Result.join ~iteration:0 left right)
        in
        ( Target.Map.merge models_left models_right ~f:merge_models,
          List.rev_append errors_left errors_right,
          Set.union skip_overrides_left skip_overrides_right,
          List.rev_append queries_left queries_right )
      in
      Scheduler.map_reduce
        scheduler
        ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
        ~initial:(Target.Map.empty, [], Ast.Reference.Set.empty, [])
        ~map
        ~reduce
        ~inputs:sources
        ()
    in
    let add_models_and_queries_from_sources () =
      let find_missing_flows =
        find_missing_flows >>= TaintConfiguration.missing_flows_kind_from_string
      in
      let taint_configuration =
        TaintConfiguration.create
          ~rule_filter
          ~find_missing_flows
          ~dump_model_query_results_path:dump_model_query_results
          ~maximum_trace_length
          ~maximum_tito_depth
          ~taint_model_paths
        |> TaintConfiguration.abort_on_error
      in
      TaintConfiguration.register taint_configuration;
      let models, errors, skip_overrides, queries =
        ModelParser.get_model_sources ~paths:taint_model_paths |> create_models ~taint_configuration
      in
      ModelVerificationError.register errors;
      let () =
        if not (List.is_empty errors) then
          (* Exit or log errors, depending on whether models need to be verified. *)
          if not verify_models then begin
            Log.error "Found %d model verification errors!" (List.length errors);
            List.iter errors ~f:(fun error -> Log.error "%s" (ModelVerificationError.display error))
          end
          else begin
            Yojson.Safe.pretty_to_string
              (`Assoc ["errors", `List (List.map errors ~f:ModelVerificationError.to_json)])
            |> Log.print "%s";
            exit (Taint.ExitStatus.exit_code Taint.ExitStatus.ModelVerificationError)
          end
      in
      {
        initialize_result =
          { Interprocedural.AnalysisResult.initial_models = models; skip_overrides };
        query_data = Some { queries; taint_configuration };
      }
    in
    let ({ initialize_result = { initial_models = user_models; _ }; _ } as result) =
      add_models_and_queries_from_sources ()
    in
    let initial_models = ClassModels.infer ~environment ~user_models in
    match taint_model_paths with
    | [] ->
        {
          initialize_result =
            {
              Interprocedural.AnalysisResult.initial_models;
              skip_overrides = Ast.Reference.Set.empty;
            };
          query_data = None;
        }
    | _ ->
        let merged_models =
          Target.Map.merge user_models initial_models ~f:(fun ~key:_ -> function
            | `Both (left, right) -> Some (Model.join left right)
            | `Left model
            | `Right model ->
                Some model)
        in
        {
          result with
          initialize_result = { result.initialize_result with initial_models = merged_models };
        }


  let initialize_models ~scheduler ~static_analysis_configuration ~environment ~callables ~stubs =
    let callables = (callables :> Target.t list) in
    let stubs = Target.HashSet.of_list (stubs :> Target.t list) in

    Log.info "Parsing taint models...";
    let timer = Timer.start () in
    let { initialize_result; query_data } =
      parse_models_and_queries_from_sources
        ~scheduler
        ~static_analysis_configuration
        ~environment
        ~callables:(Some (Target.HashSet.of_list callables))
        ~stubs
    in
    Statistics.performance ~name:"Parsed taint models" ~phase_name:"Parsing taint models" ~timer ();
    match query_data with
    | Some query_data ->
        Log.info "Generating models from model queries...";
        let timer = Timer.start () in
        let models =
          generate_models_from_queries
            ~scheduler
            ~static_analysis_configuration
            ~environment
            ~callables
            ~stubs
            ~initialize_result
            query_data
        in
        Statistics.performance
          ~name:"Generated models from model queries"
          ~phase_name:"Generating models from model queries"
          ~timer
          ();
        models
    | _ -> initialize_result


  let analyze ~environment ~callable ~qualifier ~define ~sanitizers ~modes existing_model =
    let profiler =
      if Ast.Statement.Define.dump_perf (Ast.Node.value define) then
        TaintProfiler.create ()
      else
        TaintProfiler.none
    in
    let call_graph_of_define =
      Interprocedural.CallGraph.SharedMemory.get_or_compute
        ~callable
        ~environment
        ~define:(Ast.Node.value define)
    in
    let forward, result, triggered_sinks =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis" ~f:(fun () ->
          ForwardAnalysis.run
            ~profiler
            ~environment
            ~qualifier
            ~define
            ~call_graph_of_define
            ~existing_model)
    in
    let backward =
      TaintProfiler.track_duration ~profiler ~name:"Backward analysis" ~f:(fun () ->
          BackwardAnalysis.run
            ~profiler
            ~environment
            ~qualifier
            ~define
            ~call_graph_of_define
            ~existing_model
            ~triggered_sinks)
    in
    let forward, backward =
      if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
        empty_model.forward, empty_model.backward
      else
        forward, backward
    in
    let model = { Model.forward; backward; sanitizers; modes } in
    let model =
      TaintProfiler.track_duration ~profiler ~name:"Sanitize" ~f:(fun () ->
          Model.apply_sanitizers model)
    in
    TaintProfiler.dump profiler;
    result, model


  let analyze
      ~environment
      ~callable
      ~qualifier
      ~define:
        ({ Ast.Node.value = { Ast.Statement.Define.signature = { name; _ }; _ }; _ } as define)
      ~existing
    =
    let define_qualifier = Ast.Reference.delocalize name in
    let open Analysis in
    let open Ast in
    let module_reference =
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
      let annotated_global_environment =
        GlobalResolution.annotated_global_environment global_resolution
      in
      (* Pysa inlines decorators when a function is decorated. However, we want issues and models to
         point to the lines in the module where the decorator was defined, not the module where it
         was inlined. So, look up the originating module, if any, and use that as the module
         qualifier. *)
      InlineDecorator.InlinedNameToOriginalName.get define_qualifier
      >>= AnnotatedGlobalEnvironment.ReadOnly.get_global_location annotated_global_environment
      >>| fun { Location.WithModule.path; _ } -> path
    in
    let qualifier = Option.value ~default:qualifier module_reference in
    match existing with
    | Some ({ Model.modes; _ } as model) when Model.ModeSet.contains Model.Mode.SkipAnalysis modes
      ->
        let () = Log.info "Skipping taint analysis of %a" Target.pretty_print callable in
        [], model
    | Some ({ sanitizers; modes; _ } as model) ->
        analyze ~callable ~environment ~qualifier ~define ~sanitizers ~modes model
    | None ->
        analyze
          ~callable
          ~environment
          ~qualifier
          ~define
          ~sanitizers:Model.Sanitizers.empty
          ~modes:Model.ModeSet.empty
          empty_model


  let report = Taint.Reporting.report
end)
