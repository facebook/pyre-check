(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
module Kind = AnalysisKind

(* Initialize models for the given analysis.
 * For the taint analysis, this parses taint stubs into models and queries. *)
let initialize_models kind ~scheduler ~static_analysis_configuration ~environment ~callables ~stubs =
  let (AnalysisResult.Analysis { analysis; kind = storable_kind }) =
    AnalysisResult.get_abstract_analysis kind
  in
  let module Analysis = (val analysis) in
  let specialize_models { AnalysisResult.initial_models = analysis_initial_models; skip_overrides } =
    let to_model_t model =
      let pkg = AnalysisResult.Pkg { kind = ModelPart storable_kind; value = model } in
      { AnalysisResult.models = Kind.Map.add kind pkg Kind.Map.empty; is_obscure = false }
    in
    {
      AnalysisResult.initial_models = analysis_initial_models |> Target.Map.map ~f:to_model_t;
      skip_overrides;
    }
  in
  Analysis.initialize_models
    ~static_analysis_configuration
    ~scheduler
    ~environment
    ~callables
    ~stubs
  |> specialize_models


(* Save initial models in the shared memory. *)
let record_initial_models ~callables ~stubs models =
  let record_models models =
    let add_model_to_memory ~key:call_target ~data:model =
      FixpointState.add_predefined FixpointState.Epoch.initial call_target model
    in
    Target.Map.iteri models ~f:add_model_to_memory
  in
  (* Augment models with initial inferred and obscure models *)
  let add_missing_initial_models models =
    callables
    |> List.filter ~f:(fun target -> not (Target.Map.mem models target))
    |> List.fold ~init:models ~f:(fun models target ->
           Target.Map.set models ~key:target ~data:AnalysisResult.empty_model)
  in
  let add_missing_obscure_models models =
    stubs
    |> List.filter ~f:(fun target -> not (Target.Map.mem models target))
    |> List.fold ~init:models ~f:(fun models target ->
           Target.Map.set models ~key:target ~data:AnalysisResult.obscure_model)
  in
  models |> add_missing_initial_models |> add_missing_obscure_models |> record_models


let analysis_failed step ~exn callable ~message =
  let message =
    Format.asprintf
      "%s in step %a while analyzing %a."
      message
      FixpointState.pp_step
      step
      Target.pp_pretty
      callable
  in
  Log.log_exception message exn (Worker.exception_backtrace exn);
  raise exn


let get_empty_model (type a) (kind : < model : a ; .. > AnalysisResult.storable_kind) : a =
  (* Existentially abstract unspecified analysis data *)
  let (AnalysisResult.ModelPart k1) = AnalysisResult.ModelPart kind in
  let module Analysis = (val AnalysisResult.get_analysis k1) in
  Analysis.empty_model


let get_obscure_models abstract_analysis =
  let models =
    let (AnalysisResult.Analysis { kind; analysis }) = abstract_analysis in
    let module Analysis = (val analysis) in
    let obscure_model = Analysis.obscure_model in
    Kind.Map.add
      (Kind.abstract kind)
      (AnalysisResult.Pkg { kind = ModelPart kind; value = obscure_model })
      Kind.Map.empty
  in
  AnalysisResult.{ models; is_obscure = true }


let non_fixpoint_witness
    (type a)
    (kind : < model : a ; .. > AnalysisResult.storable_kind)
    ~(iteration : int)
    ~(previous : a)
    ~(next : a)
  =
  (* Existentially abstract unspecified analysis data *)
  let (AnalysisResult.ModelPart k1) = AnalysisResult.ModelPart kind in
  let module Analysis = (val AnalysisResult.get_analysis k1) in
  if Analysis.reached_fixpoint ~iteration ~previous ~next then
    None
  else
    Some ()


(* Key is witness of non-fixpoint. *)

(* Find witnesses where next </= previous. *)
let non_fixpoint_witness ~iteration _kind left right =
  let open AnalysisResult in
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
      | Kind.Distinct -> failwith "Wrong kind matched up in fixpoint test.")


let reached_fixpoint_model_only ~iteration ~previous ~next =
  Kind.Map.merge (non_fixpoint_witness ~iteration) previous next |> Kind.Map.is_empty


let join_models ~iteration left right =
  let open AnalysisResult in
  let join_pkg
      _
      (Pkg { kind = ModelPart left_kind; value = left })
      (Pkg { kind = ModelPart right_kind; value = right })
    =
    match Kind.are_equal left_kind right_kind with
    | Kind.Equal ->
        let module Analysis = (val get_analysis left_kind) in
        Some (Pkg { kind = ModelPart left_kind; value = Analysis.join ~iteration left right })
    | Kind.Distinct -> failwith "Wrong kind matched up in join."
  in
  if phys_equal left right then
    left
  else
    Kind.Map.union join_pkg left right


let widen_models ~iteration ~previous ~next =
  let open AnalysisResult in
  let widen_pkg
      _
      (Pkg { kind = ModelPart k1; value = previous })
      (Pkg { kind = ModelPart k2; value = next })
    =
    match Kind.are_equal k1 k2 with
    | Kind.Equal ->
        let module Analysis = (val AnalysisResult.get_analysis k1) in
        Some (Pkg { kind = ModelPart k1; value = Analysis.widen ~iteration ~previous ~next })
    | Kind.Distinct -> failwith "Wrong kind matched up in widen."
  in
  if phys_equal previous next then
    previous
  else
    Kind.Map.union widen_pkg previous next


let explain_non_fixpoint ~iteration ~previous ~next =
  let open AnalysisResult in
  let witnesses = Kind.Map.merge (non_fixpoint_witness ~iteration) previous next in
  let print_witness key () =
    Log.log ~section:`Interprocedural "%s is non-fixpoint" (Kind.show key)
  in
  Kind.Map.iter print_witness witnesses


let show_models models =
  let open AnalysisResult in
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
      explain_non_fixpoint ~iteration ~previous:result ~next:previous);
    if not (reached_fixpoint_model_only ~iteration ~previous:result ~next) then (
      Log.error
        "WIDEN DOES NOT RESPECT JOIN: next = %s\nwiden = %s\n"
        (show_models next)
        (show_models result);
      explain_non_fixpoint ~iteration ~previous:result ~next);
    result
  in
  if phys_equal previous next then
    previous
  else
    verify_widen ~iteration ~previous ~next


let reached_fixpoint
    ~iteration
    ~previous:{ AnalysisResult.models = previous_model; is_obscure = previous_obscure }
    ~next:{ AnalysisResult.models = next_model; is_obscure = next_obscure }
  =
  if (not previous_obscure) && next_obscure then
    false
  else
    reached_fixpoint_model_only ~iteration ~previous:previous_model ~next:next_model


let widen
    ~iteration
    ~previous:{ AnalysisResult.models = previous_model; is_obscure = previous_obscure }
    ~next:{ AnalysisResult.models = next_model; is_obscure = next_obscure }
  =
  AnalysisResult.
    {
      is_obscure = previous_obscure || next_obscure;
      models = widen ~iteration ~previous:previous_model ~next:next_model;
    }


let widen_if_necessary step callable ~old_model ~new_model result =
  (* Check if we've reached a fixed point *)
  if reached_fixpoint ~iteration:step.FixpointState.iteration ~previous:old_model ~next:new_model
  then (
    Log.log
      ~section:`Interprocedural
      "Reached fixpoint for %a\n%a"
      Target.pp_pretty
      callable
      AnalysisResult.pp_model_t
      old_model;
    FixpointState.{ is_partial = false; model = old_model; result })
  else
    let model = widen ~iteration:step.FixpointState.iteration ~previous:old_model ~next:new_model in
    Log.log
      ~section:`Interprocedural
      "Widened fixpoint for %a\nold: %anew: %a\nwidened: %a"
      Target.pp_pretty
      callable
      AnalysisResult.pp_model_t
      old_model
      AnalysisResult.pp_model_t
      new_model
      AnalysisResult.pp_model_t
      model;
    FixpointState.{ is_partial = true; model; result }


let analyze_define step abstract_analysis callable environment qualifier define =
  let () = Log.log ~section:`Interprocedural "Analyzing %a" Target.pp_pretty callable in
  let old_model =
    match FixpointState.get_old_model callable with
    | Some model ->
        let () =
          Log.log
            ~section:`Interprocedural
            "Analyzing %a, with initial model %a"
            Target.pp_pretty
            callable
            AnalysisResult.pp_model_t
            model
        in
        model
    | None -> Format.asprintf "No initial model found for %a" Target.pp_pretty callable |> failwith
  in
  let models, results =
    try
      let (AnalysisResult.Analysis { AnalysisResult.kind; analysis }) = abstract_analysis in
      let open AnalysisResult in
      let abstract_kind = Kind.abstract kind in
      let module Analysis = (val analysis) in
      let existing = AnalysisResult.get (ModelPart kind) old_model.models in
      let result, model = Analysis.analyze ~callable ~environment ~qualifier ~define ~existing in
      ( Kind.Map.add abstract_kind (Pkg { kind = ModelPart kind; value = model }) Kind.Map.empty,
        Kind.Map.add abstract_kind (Pkg { kind = ResultPart kind; value = result }) Kind.Map.empty )
    with
    | Sys.Break as exn -> analysis_failed step ~exn ~message:"Hit Ctrl+C" callable
    | _ as exn -> analysis_failed step ~exn ~message:"Analysis failed" callable
  in
  let new_model = { AnalysisResult.models; is_obscure = false } in
  widen_if_necessary step callable ~old_model ~new_model results


let strip_for_callsite model =
  let open AnalysisResult in
  (* list them to make the type system do its work *)
  let strip abstract_kind (Pkg { kind = ModelPart kind; value = model }) models =
    let module Analysis = (val get_analysis kind) in
    let model = Analysis.strip_for_callsite model in
    Kind.Map.add abstract_kind (Pkg { kind = ModelPart kind; value = model }) models
  in
  let models = Kind.Map.fold strip model.models Kind.Map.empty in
  { model with models }


let analyze_overrides ({ FixpointState.iteration; _ } as step) callable =
  let timer = Timer.start () in
  let overrides =
    DependencyGraphSharedMemory.get_overriding_types
      ~member:(Target.get_override_reference callable)
    |> Option.value ~default:[]
    |> List.map ~f:(fun at_type -> Target.create_derived_override callable ~at_type)
  in
  let model =
    let lookup_and_join ({ AnalysisResult.is_obscure; models } as result) override =
      match FixpointState.get_model override with
      | None ->
          Log.log
            ~section:`Interprocedural
            "During override analysis, can't find model for %a"
            Target.pp_pretty
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
      FixpointState.get_model (Target.get_corresponding_method callable)
      |> Option.value ~default:AnalysisResult.empty_model
      |> strip_for_callsite
    in
    List.fold overrides ~f:lookup_and_join ~init:direct_model
  in
  let old_model =
    match FixpointState.get_old_model callable with
    | Some model -> model
    | None -> Format.asprintf "No initial model found for %a" Target.pp_pretty callable |> failwith
  in
  let state =
    widen_if_necessary step callable ~old_model ~new_model:model AnalysisResult.empty_result
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Override analysis"
    ~section:`Interprocedural
    ~normals:["callable", callable |> Target.get_override_reference |> Reference.show]
    ~timer
    ();
  state


let callables_to_dump =
  (* Set of defines we'll print models for at the end of the analysis. *)
  ref Target.Set.empty


let analyze_callable analysis step callable environment =
  let resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution environment in
  let () =
    (* Verify invariants *)
    let open FixpointState in
    match get_meta_data callable with
    | None -> ()
    | Some { step = { epoch; _ }; _ } when epoch <> step.epoch ->
        let message =
          Format.asprintf
            "Fixpoint inconsistency: callable %a analyzed during epoch %a, but stored metadata \
             from epoch %a"
            Target.pp_pretty
            callable
            Epoch.pp
            step.epoch
            Epoch.pp
            epoch
        in
        Log.error "%s" message;
        failwith message
    | _ -> ()
  in
  match callable with
  | (Target.Function _ | Target.Method _) as callable -> (
      match Target.get_module_and_definition callable ~resolution with
      | None ->
          let () = Log.error "Found no definition for %a" Target.pp_pretty callable in
          let () =
            if not (FixpointState.is_initial_iteration step) then (
              let message =
                Format.asprintf
                  "Fixpoint inconsistency: Target %a without body analyzed past initial step: %a"
                  Target.pp_pretty
                  callable
                  FixpointState.pp_step
                  step
              in
              Log.error "%s" message;
              failwith message)
          in
          FixpointState.
            {
              is_partial = false;
              model = get_obscure_models analysis;
              result = AnalysisResult.empty_result;
            }
      | Some (qualifier, ({ Node.value; _ } as define)) ->
          if Define.dump value then
            callables_to_dump := Target.Set.add callable !callables_to_dump;
          analyze_define step analysis callable environment qualifier define)
  | Target.Override _ as callable -> analyze_overrides step callable
  | Target.Object _ as path ->
      Format.asprintf "Found object %a in fixpoint analysis" Target.pp_pretty path |> failwith


type expensive_callable = {
  time_to_analyze_in_ms: int;
  callable: Target.t;
}

type result = {
  callables_processed: int;
  expensive_callables: expensive_callable list;
  callables_to_dump: Target.Set.t;
}

(* Called on a worker with a set of targets to analyze. *)
let one_analysis_pass ~analysis ~step ~environment ~callables =
  let analysis = AnalysisResult.get_abstract_analysis analysis in
  let analyze_and_cache expensive_callables callable =
    let timer = Timer.start () in
    let result = analyze_callable analysis step callable environment in
    FixpointState.add step callable result;
    (* Log outliers. *)
    let time_to_analyze_in_ms = Timer.stop_in_ms timer in
    if time_to_analyze_in_ms > 500 then begin
      Statistics.performance
        ~name:"static analysis of expensive callable"
        ~timer
        ~section:`Interprocedural
        ~normals:["callable", Target.show_pretty callable]
        ();
      { time_to_analyze_in_ms; callable } :: expensive_callables
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
  Target.Map.find dependencies callable |> Option.value ~default:[]


let compute_callables_to_reanalyze
    step
    previous_batch
    ~dependencies
    ~filtered_callables
    ~all_callables
  =
  let open FixpointState in
  let reanalyze_caller caller accumulator = Target.Set.add caller accumulator in
  let might_change_if_reanalyzed =
    List.fold previous_batch ~init:Target.Set.empty ~f:(fun accumulator callable ->
        if not (get_is_partial callable) then
          accumulator
        else
          (* c must be re-analyzed next iteration because its result has changed, and therefore its
             callers must also be reanalyzed. *)
          let callers = get_callable_dependents ~dependencies callable in
          List.fold
            callers
            ~init:(Target.Set.add callable accumulator)
            ~f:(fun accumulator caller -> reanalyze_caller caller accumulator))
  in
  (* Filter the original list in order to preserve topological sort order. *)
  let callables_to_reanalyze =
    List.filter all_callables ~f:(fun callable ->
        Target.Set.mem callable might_change_if_reanalyzed)
  in
  let () =
    if List.length callables_to_reanalyze <> Target.Set.cardinal might_change_if_reanalyzed then
      let missing =
        Target.Set.diff might_change_if_reanalyzed (Target.Set.of_list callables_to_reanalyze)
      in
      let check_missing callable =
        match get_meta_data callable with
        | None -> () (* okay, caller is in a later epoch *)
        | Some _ when Target.Set.mem callable filtered_callables ->
            (* This is fine - even though this function was called, it was filtered from the
               analysis beforehand. *)
            Log.warning
              "%a was omitted due to being explicitly filtered from the analysis."
              Target.pp_pretty
              callable
        | Some { step = { epoch; _ }; _ } when epoch = Epoch.predefined -> ()
        | Some meta ->
            let message =
              Format.asprintf
                "Re-analysis in iteration %d determined to analyze %a but it is not part of epoch \
                 %a (meta: %s)"
                step.iteration
                Target.pp_pretty
                callable
                Epoch.pp
                step.epoch
                (meta_data_to_string meta)
            in
            Log.error "%s" message;
            failwith message
      in
      Target.Set.iter check_missing missing
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
          String.concat ~sep:", " (List.map ~f:Target.show_pretty callables_to_analyze)
        else
          "..."
      in
      Log.info "Iteration #%d. %d callables [%s]" iteration number_of_callables witnesses
    in
    if number_of_callables = 0 then (* Fixpoint. *)
      iteration
    else if iteration >= max_iterations then (
      let max_to_show = 15 in
      let bucket =
        callables_to_analyze |> List.map ~f:Target.show_pretty |> List.sort ~compare:String.compare
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
      failwith message)
    else
      let timer = Timer.start () in
      let step = FixpointState.{ epoch; iteration } in
      let old_batch = FixpointState.KeySet.of_list callables_to_analyze in
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
          callables_to_dump = Target.Set.union left.callables_to_dump right.callables_to_dump;
          expensive_callables = List.rev_append left.expensive_callables right.expensive_callables;
        }
      in
      let () =
        FixpointState.oldify old_batch;
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
                callables_to_dump = Target.Set.empty;
                expensive_callables = [];
              }
            ~reduce
            ~inputs:callables_to_analyze
            ()
        in
        callables_to_dump := Target.Set.union !callables_to_dump iteration_callables_to_dump;
        FixpointState.remove_old old_batch;
        if not (List.is_empty iteration_expensive_callables) then
          Log.log
            ~section:`Performance
            "Expensive callables for iteration %d: %s"
            iteration
            (iteration_expensive_callables
            |> List.sort ~compare:(fun left right ->
                   Int.compare right.time_to_analyze_in_ms left.time_to_analyze_in_ms)
            |> List.map ~f:(fun { time_to_analyze_in_ms; callable } ->
                   Format.asprintf "`%a`: %d ms" Target.pp_pretty callable time_to_analyze_in_ms)
            |> String.concat ~sep:", ")
      in
      let callables_to_analyze =
        compute_callables_to_reanalyze
          step
          callables_to_analyze
          ~dependencies
          ~filtered_callables
          ~all_callables
      in
      Memory.SharedMemory.collect `aggressive;
      let () =
        Log.info
          "Iteration #%n, %d callables, heap size %.3fGB took %.2fs"
          iteration
          number_of_callables
          (Int.to_float (SharedMemory.heap_size ()) /. 1000000000.0)
          (Timer.stop_in_sec timer)
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
        | (Target.Function _ | Target.Method _) as callable ->
            Target.get_module_and_definition callable ~resolution
            >>| (fun (_, { Node.value; _ }) -> value)
            |> fun value -> Option.value_exn value
        | _ -> failwith "No real target to dump"
      in
      let model =
        FixpointState.get_model callable |> Option.value ~default:AnalysisResult.empty_model
      in
      Log.dump
        "Model for `%s` after %d iterations:\n%a"
        (Log.Color.yellow (Reference.show name))
        iterations
        AnalysisResult.pp_model_t
        model
    in
    Target.Set.iter dump_callable !callables_to_dump;
    iterations
  with
  | exn ->
      Log.log_exception "Fixpoint iteration failed." exn (Worker.exception_backtrace exn);
      raise exn
