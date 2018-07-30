(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core
open Hack_parallel.Std


module Callable = InterproceduralCallable
module Fixpoint = InterproceduralFixpoint
module Kind = InterproceduralAnalysisKind
module Result = InterproceduralResult


let analysis_failed step ~exn callable ~message =
  let callable = (callable :> Callable.t) in
  Log.dump "%s in step %s while analyzing %s.\nException %s\nBacktrace: %s"
    message
    (Fixpoint.show_step step)
    (Callable.show callable)
    (Exn.to_string exn)
    (Printexc.get_backtrace ());
  raise exn


let get_empty_model (type a) (kind: <model:a;..> Result.storable_kind) : a =
  (* Existentially abstract unspecified analysis data *)
  let Result.ModelPart k1 = Result.ModelPart kind in
  let module Analysis = (val (Result.get_analysis k1)) in
  Analysis.empty_model


let get_obscure_model (type a) (kind: <model:a;..> Result.storable_kind) : a =
  (* Existentially abstract unspecified analysis data *)
  let Result.ModelPart k1 = Result.ModelPart kind in
  let module Analysis = (val (Result.get_analysis k1)) in
  Analysis.obscure_model


let get_obscure_models analyses =
  let get_analysis_specific_obscure_model map abstract_analysis =
    let Result.Analysis { kind; analysis; } = abstract_analysis in
    let module Analysis = (val analysis) in
    let obscure_model = Analysis.obscure_model in
    Kind.Map.add (Kind.abstract kind) (Result.Pkg { kind = ModelPart kind; value = obscure_model; })
      map
  in
  List.fold ~f:get_analysis_specific_obscure_model ~init:Kind.Map.empty analyses


let non_fixpoint_witness
    (type a)
    (kind: <model:a;..> Result.storable_kind)
    ~(iteration:int)
    ~(previous:a)
    ~(next:a) =
  (* Existentially abstract unspecified analysis data *)
  let Result.ModelPart k1 = Result.ModelPart kind in
  let module Analysis = (val (Result.get_analysis k1)) in
  if Analysis.reached_fixpoint ~iteration ~previous ~next then None
  else Some ()  (* Key is witness of non-fixpoint. *)


(* Find witnesses where next </= previous. *)
let non_fixpoint_witness ~iteration _kind left right =
  let open Result in
  match left, right with
  | None, None -> failwith "impossible"
  | None, Some (Pkg { kind = ModelPart kind; value = next; }) ->
      let empty = get_empty_model kind in
      non_fixpoint_witness kind ~iteration ~previous:empty ~next
  | Some (Pkg { kind = ModelPart kind; value = previous}), None ->
      let empty = get_empty_model kind in
      non_fixpoint_witness kind ~iteration ~previous ~next:empty
  | Some (Pkg { kind = ModelPart k1; value = previous; }),
    Some (Pkg { kind = ModelPart k2; value = next; }) ->
      match Kind.are_equal k1 k2 with
      | Kind.Equal -> non_fixpoint_witness k1 ~iteration ~previous ~next
      | Kind.Distinct -> failwith "Wrong kind matched up in fixpoint test."


let reached_fixpoint ~iteration ~previous ~next =
  Kind.Map.merge (non_fixpoint_witness ~iteration) previous next
  |> Kind.Map.is_empty


let widen_models ~iteration ~previous ~next =
  let open Result in
  let widen_pkg _
      (Pkg { kind = ModelPart k1; value = previous; })
      (Pkg { kind = ModelPart k2; value = next; }) =
    match Kind.are_equal k1 k2 with
    | Kind.Equal ->
        let module Analysis = (val (Result.get_analysis k1)) in
        Some (Pkg { kind = ModelPart k1; value = Analysis.widen ~iteration ~previous ~next; })
    | Kind.Distinct ->
        failwith "Wrong kind matched up in widen."
  in
  if phys_equal previous next
  then previous
  else Kind.Map.union widen_pkg previous next


let explain_non_fixpoint ~iteration ~previous ~next =
  let open Result in
  let witnesses = Kind.Map.merge (non_fixpoint_witness ~iteration) previous next in
  let print_witness key () = Log.dump "%s is non-fixpoint" (Kind.show key)
  in
  Kind.Map.iter print_witness witnesses


let show_models models =
  let open Result in
  (* list them to make the type system do its work *)
  let to_string (akind, Pkg { kind = ModelPart kind; value = model; }) =
    let module Analysis = (val (get_analysis kind))
    in
    Format.sprintf "%s: %s" (Kind.show akind) (Analysis.show_call_model model)
  in
  Kind.Map.bindings models
  |> List.map ~f:to_string
  |> String.concat ~sep:"\n"


let widen ~iteration ~previous ~next =
  let verify_widen ~iteration ~previous ~next =
    let result = widen_models ~iteration ~previous ~next in
    if not (reached_fixpoint ~iteration ~previous:result ~next:previous) then
      begin
        Log.dump "WIDEN DOES NOT RESPECT JOIN: previous = %s\nwiden = %s\n"
          (show_models previous) (show_models result);
        explain_non_fixpoint ~iteration ~previous:result ~next:previous
      end;
    if not (reached_fixpoint ~iteration ~previous:result ~next:next) then
      begin
        Log.dump "WIDEN DOES NOT RESPECT JOIN: next = %s\nwiden = %s\n"
          (show_models next) (show_models result);
        explain_non_fixpoint ~iteration ~previous:result ~next:next
      end;
    result
  in
  if phys_equal previous next then
    previous
  else
    verify_widen ~iteration ~previous ~next


let widen_if_necessary step callable new_model result =
  (* Check if we've reached a fixed point *)
  match Fixpoint.get_old_model callable with
  | None ->
      Fixpoint.{ is_partial = true; model = new_model; result; }
  | Some old_model ->
      if reached_fixpoint ~iteration:step.Fixpoint.iteration
          ~previous:old_model ~next:new_model then
        Fixpoint.{ is_partial = false; model = old_model; result }
      else
        Fixpoint.{
          is_partial = true;
          model =
            widen ~iteration:step.Fixpoint.iteration ~previous:old_model ~next:new_model;
          result;
        }


let analyze_define step analyses callable define =
  let analyze (Result.Analysis { Result.kind; analysis; }) =
    let open Result in
    let akind = Kind.abstract kind in
    let module Analysis = (val analysis) in
    let method_result, method_model = Analysis.analyze callable define in
    (
      akind,
      Pkg { kind = ModelPart kind; value = method_model; },
      Pkg { kind = ResultPart kind; value = method_result; }
    )
  in
  let new_model, results =
    let accumulate (models, results) analysis =
      let akind, model, result = analyze analysis
      in
      Result.Kind.Map.add akind model models,
      Result.Kind.Map.add akind result results
    in
    try
      (* Run all the analyses *)
      let init = Result.Kind.Map.(empty, empty) in
      let models, results = List.fold ~f:accumulate analyses ~init in
      models, results
    with
    | Sys.Break as exn ->
        analysis_failed step ~exn ~message:"Hit Ctrl+C" callable
    | _ as exn ->
        analysis_failed step ~exn ~message:"Analysis failed" callable
  in
  widen_if_necessary step callable new_model results


let analyze_overrides step callable =
  Fixpoint.{
    is_partial = false;
    model = Result.empty_model;
    result = Result.empty_result;
  }


let analyze_callable analyses step callable =
  let () = (* Verify invariants *)
    let open Fixpoint in
    match Fixpoint.get_meta_data callable with
    | None -> ()
    | Some { step = { epoch; _ }; _ } when epoch <> step.epoch ->
        let message =
          Format.sprintf "Fixpoint inconsistency: callable %s analyzed during epoch %s, but stored \
                          metadata from epoch %s"
            (Callable.show callable)
            (Fixpoint.Epoch.show step.epoch)
            (Fixpoint.Epoch.show epoch)
        in
        Log.dump "%s" message;
        failwith message
    | _ -> ()
  in
  match callable with
  | #Callable.real_target as callable ->
      begin
        match Callable.get_definition callable with
        | None ->
            let () = Log.dump "Found no definition for %s" (Callable.show callable) in
            let () =
              if not (Fixpoint.is_initial_iteration step) then
                let message =
                  Format.sprintf "Fixpoint inconsistency: Callable %s without body analyzed past \
                                  initial step: %s"
                    (Callable.show callable)
                    (Fixpoint.show_step step)
                in
                Log.dump "%s" message;
                failwith message
            in
            Fixpoint.{
              is_partial = false;
              model = get_obscure_models analyses;
              result = Result.empty_result;
            }
        | Some { Ast.Node.value = define; _ } ->
            analyze_define step analyses callable define
      end
  | #Callable.override_target as callable ->
      analyze_overrides step callable


let errors results =
  let open Result in
  let get_diagnostics (Pkg { kind = ResultPart kind; value; }) =
    let module Analysis = (val (get_analysis kind)) in
    Analysis.get_errors value
  in
  Kind.Map.bindings results
  |> List.map ~f:snd
  |> List.concat_no_order
  |> List.map ~f:get_diagnostics


let summaries_internal callable models results =
  let open Result in
  let merge kind model_opt result_opt =
    match model_opt with
    | Some model ->
        Some (model, result_opt)
    | None ->
        None
  in
  let merged = Kind.Map.merge merge models results in
  let get_summaries (_key, (Pkg { kind = ModelPart kind1; value = model; }, result_option)) =
    match result_option with
    | None ->
        let module Analysis = (val (Result.get_analysis kind1)) in
        Analysis.summaries callable None model
    | Some (Pkg { kind = ResultPart kind2; value = result; }) ->
        match Result.Kind.are_equal kind1 kind2 with
        | Kind.Equal ->
            let module Analysis = (val (Result.get_analysis kind1)) in
            Analysis.summaries callable (Some result) model
        | Kind.Distinct -> failwith "kind mismatch"
  in
  Kind.Map.bindings merged
  |> List.concat_map ~f:get_summaries


let summaries callable =
  match Fixpoint.get_model callable with
  | Some models ->
      let results = Fixpoint.get_result callable in
      summaries_internal callable models results
  | None ->
      []


(* Called on a worker with a set of functions to analyze. *)
let one_analysis_pass ~analyses step ~schedule =
  let analyses = List.map ~f:Result.get_abstract_analysis analyses in
  let analyze_and_cache callable =
    let result = analyze_callable analyses step callable in
    Fixpoint.add_state step callable result
  in
  List.iter schedule ~f:analyze_and_cache


let make_n_list_buckets ~buckets work =
  let splitter work buckets =
    let work = Array.of_list work in
    let bucket_size = Array.length work / buckets in
    let remainder = Array.length work mod buckets in
    let index = ref 0
    in
    fun ~bucket ->
      let bucket_size = min (Array.length work - !index)
          (bucket_size + if bucket < remainder then 1 else 0) in
      let result = Array.sub work ~pos:!index ~len:bucket_size in
      index := bucket_size + !index;
      Array.to_list result
  in
  Bucket.make_n_buckets ~buckets:buckets ~split:(splitter work buckets)


let get_callable_dependents ~caller_map = function
  | #Callable.real_target as real ->
      begin
        match Ast.Expression.Access.Map.find caller_map (Callable.get_real_access real) with
        | None -> []
        | Some callers ->
            List.map ~f:Callable.make_real callers
      end
  | #Callable.override_target as _override ->
      (* TODO(T32010422) *)
      []


let compute_callables_to_reanalyze step previous_batch ~caller_map ~all_callables =
  let open Fixpoint in
  let reanalyze_caller caller accumulator =
    Callable.Set.add caller accumulator
  in
  let might_change_if_reanalyzed =
    List.fold previous_batch ~init:Callable.Set.empty
      ~f:(fun accumulator callable ->
          if not (Fixpoint.get_is_partial callable) then
            accumulator
          else
            (* c must be re-analyzed next iteration because its result has
               changed, and therefore its callers must also be reanalyzed. *)
            let callers = get_callable_dependents ~caller_map callable in
            List.fold callers ~init:(Callable.Set.add callable accumulator)
              ~f:(fun accumulator caller -> reanalyze_caller caller accumulator))
  in
  (* Filter the original list in order to preserve topological sort order. *)
  let callables_to_reanalyze =
    List.filter
      all_callables
      ~f:(fun callable -> Callable.Set.mem callable might_change_if_reanalyzed)
  in
  let () =
    if List.length callables_to_reanalyze <> Callable.Set.cardinal might_change_if_reanalyzed then
      let missing =
        Callable.Set.diff might_change_if_reanalyzed (Callable.Set.of_list callables_to_reanalyze)
      in
      let check_missing callable =
        match Fixpoint.get_meta_data callable with
        | None -> () (* okay, caller is in a later epoch *)
        | Some meta ->
            let message =
              Format.sprintf
                "Re-analysis in iteration %d determined to analyze %s but it is not \
                 part of epoch %s (meta: %s)"
                step.iteration
                (Callable.show callable)
                (Fixpoint.Epoch.show step.epoch)
                (Fixpoint.meta_data_to_string meta)
            in
            Log.dump "%s" message;
            failwith message
      in
      Callable.Set.iter check_missing missing
  in
  callables_to_reanalyze


let compute_fixpoint ~workers ~analyses ~caller_map ~all_callables epoch =
  (* Start iteration > 0 is to avoid a useless special 0 iteration for mega
     components. *)
  let max_iterations = 100 in
  let bucket_size = 10_000 in
  let num_workers = match workers with None -> 1 | Some w -> List.length w in
  let rec iterate ~iteration callables_to_analyze =
    let num_callables = List.length callables_to_analyze in
    let () =
      let witnesses =
        if num_callables <= 6 then
          String.concat ~sep:", " (List.map ~f:Callable.show callables_to_analyze)
        else
          "..."
      in
      Log.dump "Iteration #%d. Callables [%s]" iteration witnesses
    in
    if num_callables = 0 then
      (* Fixpoint. *)
      iteration
    else if iteration >= max_iterations then
      begin
        Log.dump "Failed to reach interprocedural fixed point";
        let max_to_show = 15 in
        let bucket =
          callables_to_analyze
          |> List.map ~f:Callable.show
          |> List.sort ~compare:String.compare
        in
        let bucket_len = List.length bucket in
        let message =
          Format.sprintf
            "For a bucket of %d callables: %s%s"
            bucket_len
            (String.concat ~sep:", " (List.take bucket max_to_show))
            (if bucket_len > max_to_show then "..." else "")
        in
        Log.dump "%s" message;
        failwith message
      end
    else
      let time_0 = Unix.gettimeofday () in
      let buckets = max (num_callables / bucket_size) (num_workers * 5) in
      let step = Fixpoint.{ epoch; iteration; } in
      let old_batch = Fixpoint.KeySet.of_list callables_to_analyze in
      let () =
        Fixpoint.oldify old_batch;
        MultiWorker.call
          workers
          ~job:(fun _ Bucket.{ work = schedule; _ } -> one_analysis_pass ~analyses step ~schedule)
          ~neutral: ()
          ~merge:(fun _ _ -> ())
          ~next:(make_n_list_buckets ~buckets callables_to_analyze);
        Fixpoint.remove_old old_batch
      in
      let callables_to_analyze =
        compute_callables_to_reanalyze step callables_to_analyze ~caller_map ~all_callables
      in
      let hs = SharedMem.heap_size () in
      let time_f = Unix.gettimeofday () in
      let elapsed = time_f -. time_0 |> Unix.gmtime in
      let () =
        Log.dump "Iteration #%n, heap size %n took %nm %02ds"
          iteration hs elapsed.Unix.tm_min elapsed.Unix.tm_sec
      in
      iterate ~iteration:(iteration + 1) callables_to_analyze
  in
  iterate ~iteration:0 all_callables
