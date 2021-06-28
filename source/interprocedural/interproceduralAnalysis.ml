(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
module Kind = AnalysisKind
module Result = InterproceduralResult

let initialize_configuration kind ~static_analysis_configuration =
  let (Result.Analysis { analysis; _ }) = Result.get_abstract_analysis kind in
  let module Analysis = (val analysis) in
  Analysis.initialize_configuration ~static_analysis_configuration


(* Initialize models for the given analysis.
 * For the taint analysis, this parses taint stubs into models and queries. *)
let initialize_models kind ~scheduler ~static_analysis_configuration ~environment ~functions ~stubs =
  let (Result.Analysis { analysis; kind = storable_kind }) = Result.get_abstract_analysis kind in
  let module Analysis = (val analysis) in
  (* We call initialize_models outside the returned lambda so that initial models are computed
     eagerly. *)
  let initialized_models =
    Analysis.initialize_models
      ~static_analysis_configuration
      ~scheduler
      ~environment
      ~functions
      ~stubs
  in
  let specialize_models
      { Result.InitializedModels.initial_models = analysis_initial_models; skip_overrides }
    =
    let to_model_t model =
      let pkg = Result.Pkg { kind = ModelPart storable_kind; value = model } in
      { Result.models = Kind.Map.add kind pkg Kind.Map.empty; is_obscure = false }
    in
    {
      Result.InitializedModels.initial_models =
        analysis_initial_models |> Callable.Map.map ~f:to_model_t;
      skip_overrides;
    }
  in
  let get_specialized_models ~updated_environment =
    (* We call get_models_including_generated_models within the lambda so that callable-specific
       models are generated only on demand. *)
    Result.InitializedModels.get_models_including_generated_models
      ~updated_environment
      initialized_models
    |> specialize_models
  in
  Result.InitializedModels.create get_specialized_models


(* Save initial models in the shared memory. *)
let record_initial_models ~functions ~stubs models =
  let record_models models =
    let add_model_to_memory ~key:call_target ~data:model =
      Fixpoint.add_predefined Fixpoint.Epoch.initial call_target model
    in
    Callable.Map.iteri models ~f:add_model_to_memory
  in
  (* Augment models with initial inferred and obscure models *)
  let add_missing_initial_models models =
    List.filter functions ~f:(fun callable -> not (Callable.Map.mem models callable))
    |> List.fold ~init:models ~f:(fun models callable ->
           Callable.Map.set models ~key:callable ~data:Result.empty_model)
  in
  let add_missing_obscure_models models =
    List.filter stubs ~f:(fun callable -> not (Callable.Map.mem models callable))
    |> List.fold ~init:models ~f:(fun models callable ->
           Callable.Map.set models ~key:callable ~data:Result.obscure_model)
  in
  models |> add_missing_initial_models |> add_missing_obscure_models |> record_models


let analysis_failed step ~exn callable ~message =
  let callable = (callable :> Callable.t) in
  Log.error
    "%s in step %s while analyzing %s.\nException %s\nBacktrace: %s"
    message
    (Fixpoint.show_step step)
    (Callable.show callable)
    (Exn.to_string exn)
    (Printexc.get_backtrace ());
  raise exn


let get_empty_model (type a) (kind : < model : a ; .. > Result.storable_kind) : a =
  (* Existentially abstract unspecified analysis data *)
  let (Result.ModelPart k1) = Result.ModelPart kind in
  let module Analysis = (val Result.get_analysis k1) in
  Analysis.empty_model


let get_obscure_models abstract_analysis =
  let models =
    let (Result.Analysis { kind; analysis }) = abstract_analysis in
    let module Analysis = (val analysis) in
    let obscure_model = Analysis.obscure_model in
    Kind.Map.add
      (Kind.abstract kind)
      (Result.Pkg { kind = ModelPart kind; value = obscure_model })
      Kind.Map.empty
  in
  Result.{ models; is_obscure = true }


let non_fixpoint_witness
    (type a)
    (kind : < model : a ; .. > Result.storable_kind)
    ~(iteration : int)
    ~(previous : a)
    ~(next : a)
  =
  (* Existentially abstract unspecified analysis data *)
  let (Result.ModelPart k1) = Result.ModelPart kind in
  let module Analysis = (val Result.get_analysis k1) in
  if Analysis.reached_fixpoint ~iteration ~previous ~next then
    None
  else
    Some ()


(* Key is witness of non-fixpoint. *)

(* Find witnesses where next </= previous. *)
let non_fixpoint_witness ~iteration _kind left right =
  let open Result in
  match left, right with
  | None, None -> failwith "impossible"
  | None, Some (Pkg { kind = ModelPart kind; value = next }) ->
      let empty = get_empty_model kind in
      non_fixpoint_witness kind ~iteration ~previous:empty ~next
  | Some (Pkg { kind = ModelPart kind; value = previous }), None ->
      let empty = get_empty_model kind in
      non_fixpoint_witness kind ~iteration ~previous ~next:empty
  | ( Some (Pkg { kind = ModelPart k1; value = previous }),
      Some (Pkg { kind = ModelPart k2; value = next }) ) -> (
      match Kind.are_equal k1 k2 with
      | Kind.Equal -> non_fixpoint_witness k1 ~iteration ~previous ~next
      | Kind.Distinct -> failwith "Wrong kind matched up in fixpoint test." )


let reached_fixpoint_model_only ~iteration ~previous ~next =
  Kind.Map.merge (non_fixpoint_witness ~iteration) previous next |> Kind.Map.is_empty


let join_models ~iteration left right =
  let open Result in
  let join_pkg
      _
      (Pkg { kind = ModelPart left_kind; value = left })
      (Pkg { kind = ModelPart right_kind; value = right })
    =
    match Kind.are_equal left_kind right_kind with
    | Kind.Equal ->
        let module Analysis = (val Result.get_analysis left_kind) in
        Some (Pkg { kind = ModelPart left_kind; value = Analysis.join ~iteration left right })
    | Kind.Distinct -> failwith "Wrong kind matched up in join."
  in
  if phys_equal left right then
    left
  else
    Kind.Map.union join_pkg left right


let widen_models ~iteration ~previous ~next =
  let open Result in
  let widen_pkg
      _
      (Pkg { kind = ModelPart k1; value = previous })
      (Pkg { kind = ModelPart k2; value = next })
    =
    match Kind.are_equal k1 k2 with
    | Kind.Equal ->
        let module Analysis = (val Result.get_analysis k1) in
        Some (Pkg { kind = ModelPart k1; value = Analysis.widen ~iteration ~previous ~next })
    | Kind.Distinct -> failwith "Wrong kind matched up in widen."
  in
  if phys_equal previous next then
    previous
  else
    Kind.Map.union widen_pkg previous next


let explain_non_fixpoint ~iteration ~previous ~next =
  let open Result in
  let witnesses = Kind.Map.merge (non_fixpoint_witness ~iteration) previous next in
  let print_witness key () =
    Log.log ~section:`Interprocedural "%s is non-fixpoint" (Kind.show key)
  in
  Kind.Map.iter print_witness witnesses


let show_models models =
  let open Result in
  (* list them to make the type system do its work *)
  let to_string (abstract_kind, Pkg { kind = ModelPart kind; value = model }) =
    let module Analysis = (val get_analysis kind) in
    Format.sprintf "%s: %s" (Kind.show abstract_kind) (Analysis.show_call_model model)
  in
  Kind.Map.bindings models |> List.map ~f:to_string |> String.concat ~sep:"\n"


let widen ~iteration ~previous ~next =
  let verify_widen ~iteration ~previous ~next =
    let result = widen_models ~iteration ~previous ~next in
    if not (reached_fixpoint_model_only ~iteration ~previous:result ~next:previous) then (
      Log.error
        "WIDEN DOES NOT RESPECT JOIN: previous = %s\nwiden = %s\n"
        (show_models previous)
        (show_models result);
      explain_non_fixpoint ~iteration ~previous:result ~next:previous );
    if not (reached_fixpoint_model_only ~iteration ~previous:result ~next) then (
      Log.error
        "WIDEN DOES NOT RESPECT JOIN: next = %s\nwiden = %s\n"
        (show_models next)
        (show_models result);
      explain_non_fixpoint ~iteration ~previous:result ~next );
    result
  in
  if phys_equal previous next then
    previous
  else
    verify_widen ~iteration ~previous ~next


let reached_fixpoint
    ~iteration
    ~previous:{ Result.models = previous_model; is_obscure = previous_obscure }
    ~next:{ Result.models = next_model; is_obscure = next_obscure }
  =
  if (not previous_obscure) && next_obscure then
    false
  else
    reached_fixpoint_model_only ~iteration ~previous:previous_model ~next:next_model


let widen
    ~iteration
    ~previous:{ Result.models = previous_model; is_obscure = previous_obscure }
    ~next:{ Result.models = next_model; is_obscure = next_obscure }
  =
  Result.
    {
      is_obscure = previous_obscure || next_obscure;
      models = widen ~iteration ~previous:previous_model ~next:next_model;
    }


let widen_if_necessary step callable ~old_model ~new_model result =
  (* Check if we've reached a fixed point *)
  if reached_fixpoint ~iteration:step.Fixpoint.iteration ~previous:old_model ~next:new_model then (
    Log.log
      ~section:`Interprocedural
      "Reached fixpoint for %a\n%a"
      Callable.pretty_print
      callable
      Result.pp_model_t
      old_model;
    Fixpoint.{ is_partial = false; model = old_model; result } )
  else
    let model = widen ~iteration:step.Fixpoint.iteration ~previous:old_model ~next:new_model in
    Log.log
      ~section:`Interprocedural
      "Widened fixpoint for %a\nold: %anew: %a\nwidened: %a"
      Callable.pretty_print
      callable
      Result.pp_model_t
      old_model
      Result.pp_model_t
      new_model
      Result.pp_model_t
      model;
    Fixpoint.{ is_partial = true; model; result }


let analyze_define
    step
    abstract_analysis
    callable
    environment
    qualifier
    ({ Node.value = { Define.signature = { name; _ }; _ }; _ } as define)
  =
  let () = Log.log ~section:`Interprocedural "Analyzing %a" Callable.pp_real_target callable in
  let old_model =
    match Fixpoint.get_old_model callable with
    | Some model ->
        let () =
          Log.log
            ~section:`Interprocedural
            "Analyzing %a, with initial model %a"
            Callable.pp_real_target
            callable
            Result.pp_model_t
            model
        in
        model
    | None ->
        Format.asprintf "No initial model found for %a" Callable.pretty_print callable |> failwith
  in
  let models, results =
    try
      let (Result.Analysis { Result.kind; analysis }) = abstract_analysis in
      let open Result in
      let abstract_kind = Kind.abstract kind in
      let module Analysis = (val analysis) in
      let existing = Result.get (ModelPart kind) old_model.models in
      let result, model = Analysis.analyze ~callable ~environment ~qualifier ~define ~existing in
      ( Kind.Map.add abstract_kind (Pkg { kind = ModelPart kind; value = model }) Kind.Map.empty,
        Kind.Map.add abstract_kind (Pkg { kind = ResultPart kind; value = result }) Kind.Map.empty )
    with
    | Analysis.ClassHierarchy.Untracked annotation ->
        Log.log
          ~section:`Info
          "Could not generate model for `%a` due to invalid annotation `%s`"
          Reference.pp
          (Node.value name)
          annotation;
        Result.Kind.Map.empty, Result.Kind.Map.empty
    | Sys.Break as exn -> analysis_failed step ~exn ~message:"Hit Ctrl+C" callable
    | _ as exn -> analysis_failed step ~exn ~message:"Analysis failed" callable
  in
  let new_model = { Result.models; is_obscure = false } in
  widen_if_necessary step callable ~old_model ~new_model results


let strip_for_callsite model =
  let open Result in
  (* list them to make the type system do its work *)
  let strip abstract_kind (Pkg { kind = ModelPart kind; value = model }) models =
    let module Analysis = (val get_analysis kind) in
    let model = Analysis.strip_for_callsite model in
    Kind.Map.add abstract_kind (Pkg { kind = ModelPart kind; value = model }) models
  in
  let models = Kind.Map.fold strip model.InterproceduralResult.models Kind.Map.empty in
  { model with models }


let analyze_overrides ({ Fixpoint.iteration; _ } as step) callable =
  let overrides =
    DependencyGraphSharedMemory.get_overriding_types
      ~member:(Callable.get_override_reference callable)
    |> Option.value ~default:[]
    |> List.map ~f:(fun at_type -> Callable.create_derived_override callable ~at_type)
  in
  let model =
    let lookup_and_join ({ Result.is_obscure; models } as result) override =
      match Fixpoint.get_model override with
      | None ->
          Log.log
            ~section:`Interprocedural
            "During override analysis, can't find model for %a"
            Callable.pretty_print
            override;
          result
      | Some model ->
          let model = strip_for_callsite model in
          {
            is_obscure = is_obscure || model.is_obscure;
            models = join_models ~iteration models model.models;
          }
    in
    let direct_model =
      Fixpoint.get_model (Callable.get_corresponding_method callable)
      |> Option.value ~default:Result.empty_model
      |> strip_for_callsite
    in
    List.fold overrides ~f:lookup_and_join ~init:direct_model
  in
  let old_model =
    match Fixpoint.get_old_model callable with
    | Some model -> model
    | None ->
        Format.asprintf "No initial model found for %a" Callable.pretty_print callable |> failwith
  in
  widen_if_necessary step callable ~old_model ~new_model:model Result.empty_result


let callables_to_dump =
  (* Set of defines we'll print models for at the end of the analysis. *)
  ref Callable.Set.empty


let analyze_callable analysis step callable environment =
  let resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution environment in
  let () =
    (* Verify invariants *)
    let open Fixpoint in
    match Fixpoint.get_meta_data callable with
    | None -> ()
    | Some { step = { epoch; _ }; _ } when epoch <> step.epoch ->
        let message =
          Format.sprintf
            "Fixpoint inconsistency: callable %s analyzed during epoch %s, but stored metadata \
             from epoch %s"
            (Callable.show callable)
            (Fixpoint.Epoch.show step.epoch)
            (Fixpoint.Epoch.show epoch)
        in
        Log.error "%s" message;
        failwith message
    | _ -> ()
  in
  match callable with
  | #Callable.real_target as callable -> (
      match Callable.get_module_and_definition callable ~resolution with
      | None ->
          let () = Log.error "Found no definition for %s" (Callable.show callable) in
          let () =
            if not (Fixpoint.is_initial_iteration step) then (
              let message =
                Format.sprintf
                  "Fixpoint inconsistency: Callable %s without body analyzed past initial step: %s"
                  (Callable.show callable)
                  (Fixpoint.show_step step)
              in
              Log.error "%s" message;
              failwith message )
          in
          Fixpoint.
            {
              is_partial = false;
              model = get_obscure_models analysis;
              result = Result.empty_result;
            }
      | Some (qualifier, ({ Node.value; _ } as define)) ->
          if Define.dump value then
            callables_to_dump := Callable.Set.add callable !callables_to_dump;
          analyze_define step analysis callable environment qualifier define )
  | #Callable.override_target as callable -> analyze_overrides step callable
  | #Callable.object_target as path ->
      Format.asprintf "Found object %a in fixpoint analysis" Callable.pp path |> failwith


type expensive_callable = {
  time_to_analyze_in_ms: int;
  callable: Callable.t;
}

type result = {
  callables_processed: int;
  expensive_callables: expensive_callable list;
  callables_to_dump: Callable.Set.t;
}

(* Called on a worker with a set of functions to analyze. *)
let one_analysis_pass ~analysis ~step ~environment ~callables =
  let analysis = Result.get_abstract_analysis analysis in
  let analyze_and_cache expensive_callables callable =
    let timer = Timer.start () in
    let result = analyze_callable analysis step callable environment in
    Fixpoint.add_state step callable result;
    (* Log outliers. *)
    if Timer.stop_in_ms timer > 500 then begin
      Statistics.performance
        ~name:"static analysis of expensive callable"
        ~timer
        ~section:`Interprocedural
        ~normals:["callable", Callable.show callable]
        ();
      { time_to_analyze_in_ms = Timer.stop_in_ms timer; callable } :: expensive_callables
    end
    else
      expensive_callables
  in
  let expensive_callables = List.fold callables ~f:analyze_and_cache ~init:[] in
  {
    callables_processed = List.length callables;
    callables_to_dump = !callables_to_dump;
    expensive_callables;
  }


let get_callable_dependents ~dependencies callable =
  Callable.Map.find dependencies callable |> Option.value ~default:[]


let compute_callables_to_reanalyze
    step
    previous_batch
    ~dependencies
    ~filtered_callables
    ~all_callables
  =
  let open Fixpoint in
  let reanalyze_caller caller accumulator = Callable.Set.add caller accumulator in
  let might_change_if_reanalyzed =
    List.fold previous_batch ~init:Callable.Set.empty ~f:(fun accumulator callable ->
        if not (Fixpoint.get_is_partial callable) then
          accumulator
        else
          (* c must be re-analyzed next iteration because its result has changed, and therefore its
             callers must also be reanalyzed. *)
          let callers = get_callable_dependents ~dependencies callable in
          List.fold
            callers
            ~init:(Callable.Set.add callable accumulator)
            ~f:(fun accumulator caller -> reanalyze_caller caller accumulator))
  in
  (* Filter the original list in order to preserve topological sort order. *)
  let callables_to_reanalyze =
    List.filter all_callables ~f:(fun callable ->
        Callable.Set.mem callable might_change_if_reanalyzed)
  in
  let () =
    if List.length callables_to_reanalyze <> Callable.Set.cardinal might_change_if_reanalyzed then
      let missing =
        Callable.Set.diff might_change_if_reanalyzed (Callable.Set.of_list callables_to_reanalyze)
      in
      let check_missing callable =
        match Fixpoint.get_meta_data callable with
        | None -> () (* okay, caller is in a later epoch *)
        | Some _ when Callable.Set.mem callable filtered_callables ->
            (* This is fine - even though this function was called, it was filtered from the
               analysis beforehand. *)
            Log.warning
              "%s was omitted due to being explicitly filtered from the analysis."
              (Callable.show callable)
        | Some { step = { epoch; _ }; _ } when epoch = Epoch.predefined -> ()
        | Some meta ->
            let message =
              Format.sprintf
                "Re-analysis in iteration %d determined to analyze %s but it is not part of epoch \
                 %s (meta: %s)"
                step.iteration
                (Callable.show callable)
                (Fixpoint.Epoch.show step.epoch)
                (Fixpoint.meta_data_to_string meta)
            in
            Log.error "%s" message;
            failwith message
      in
      Callable.Set.iter check_missing missing
  in
  callables_to_reanalyze


let compute_fixpoint
    ~scheduler
    ~environment
    ~analysis
    ~dependencies
    ~filtered_callables
    ~all_callables
    epoch
  =
  (* Start iteration > 0 is to avoid a useless special 0 iteration for mega components. *)
  let max_iterations = 100 in
  let rec iterate ~iteration callables_to_analyze =
    let number_of_callables = List.length callables_to_analyze in
    let () =
      let witnesses =
        if number_of_callables <= 6 then
          String.concat ~sep:", " (List.map ~f:Callable.show callables_to_analyze)
        else
          "..."
      in
      Log.log
        ~section:`Info
        "Iteration #%d. %d Callables [%s]"
        iteration
        number_of_callables
        witnesses
    in
    if number_of_callables = 0 then (* Fixpoint. *)
      iteration
    else if iteration >= max_iterations then (
      let max_to_show = 15 in
      let bucket =
        callables_to_analyze |> List.map ~f:Callable.show |> List.sort ~compare:String.compare
      in
      let bucket_len = List.length bucket in
      let message =
        Format.sprintf
          "For a bucket of %d callables: %s%s"
          bucket_len
          (String.concat ~sep:", " (List.take bucket max_to_show))
          (if bucket_len > max_to_show then "..." else "")
      in
      Log.error "%s" message;
      failwith message )
    else
      let timer = Timer.start () in
      let step = Fixpoint.{ epoch; iteration } in
      let old_batch = Fixpoint.KeySet.of_list callables_to_analyze in
      let reduce left right =
        let callables_processed = left.callables_processed + right.callables_processed in
        let () =
          Log.log
            ~section:`Progress
            "Processed %d of %d callables"
            callables_processed
            number_of_callables
        in
        {
          callables_processed;
          callables_to_dump = Callable.Set.union left.callables_to_dump right.callables_to_dump;
          expensive_callables = List.rev_append left.expensive_callables right.expensive_callables;
        }
      in
      let () =
        Fixpoint.oldify old_batch;
        let {
          callables_to_dump = iteration_callables_to_dump;
          expensive_callables = iteration_expensive_callables;
          _;
        }
          =
          Scheduler.map_reduce
            scheduler
            ~policy:(Scheduler.Policy.legacy_fixed_chunk_size 1000)
            ~map:(fun _ callables -> one_analysis_pass ~analysis ~step ~environment ~callables)
            ~initial:
              {
                callables_processed = 0;
                callables_to_dump = Callable.Set.empty;
                expensive_callables = [];
              }
            ~reduce
            ~inputs:callables_to_analyze
            ()
        in
        callables_to_dump := Callable.Set.union !callables_to_dump iteration_callables_to_dump;
        Fixpoint.remove_old old_batch;
        if not (List.is_empty iteration_expensive_callables) then
          Log.log
            ~section:`Performance
            "Expensive callables for iteration %d: %s"
            iteration
            ( iteration_expensive_callables
            |> List.sort ~compare:(fun left right ->
                   Int.compare right.time_to_analyze_in_ms left.time_to_analyze_in_ms)
            |> List.map ~f:(fun { time_to_analyze_in_ms; callable } ->
                   Format.sprintf "`%s`: %d ms" (Callable.show callable) time_to_analyze_in_ms)
            |> String.concat ~sep:", " )
      in
      let callables_to_analyze =
        compute_callables_to_reanalyze
          step
          callables_to_analyze
          ~dependencies
          ~filtered_callables
          ~all_callables
      in
      let () =
        Log.info
          "Iteration #%n, %d callables, heap size %.3fGB took %.2fs"
          iteration
          number_of_callables
          (Int.to_float (SharedMem.heap_size ()) /. 1000000000.0)
          (Timer.stop timer |> Time.Span.to_sec)
      in
      iterate ~iteration:(iteration + 1) callables_to_analyze
  in
  try
    let iterations = iterate ~iteration:0 all_callables in
    let dump_callable callable =
      let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution environment in
      let resolution =
        Analysis.TypeCheck.resolution
          global_resolution
          (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
          (module Analysis.TypeCheck.DummyContext)
      in

      let resolution = Analysis.Resolution.global_resolution resolution in
      let { Define.signature = { name; _ }; _ } =
        match callable with
        | #Callable.real_target as callable ->
            Callable.get_module_and_definition callable ~resolution
            >>| (fun (_, { Node.value; _ }) -> value)
            |> fun value -> Option.value_exn value
        | _ -> failwith "No real target to dump"
      in
      let model = Fixpoint.get_model callable |> Option.value ~default:Result.empty_model in
      Log.dump
        "Model for `%s` after %d iterations:\n%a"
        (Log.Color.yellow (Reference.show (Node.value name)))
        iterations
        Result.pp_model_t
        model
    in
    Callable.Set.iter dump_callable !callables_to_dump;
    iterations
  with
  | exn ->
      Log.error
        "Fixpoint iteration failed.\nException %s\nBacktrace: %s"
        (Exn.to_string exn)
        (Printexc.get_backtrace ());
      raise exn


let report_results
    ~scheduler
    ~static_analysis_configuration
    ~environment
    ~filename_lookup
    ~analysis
    ~callables
    ~skipped_overrides
    ~fixpoint_timer
    ~fixpoint_iterations
  =
  let report_analysis (Result.Analysis { analysis; _ }) =
    let module Analysis = (val analysis) in
    Analysis.report
      ~scheduler
      ~static_analysis_configuration
      ~environment
      ~filename_lookup
      ~callables
      ~skipped_overrides
      ~fixpoint_timer
      ~fixpoint_iterations
  in
  Result.get_abstract_analysis analysis |> report_analysis
