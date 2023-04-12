(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Target = Interprocedural.Target

(* A map from issue handles to issues. *)
let issue_handle_map ~callables ~fixpoint_state =
  let union handle _ _ =
    Format.asprintf "Multiple issues share the same issue handle: %s" (IssueHandle.show handle)
    |> failwith
  in
  let accumulate_issue_handles issue_handle_map callable =
    Fixpoint.get_result fixpoint_state callable
    |> IssueHandle.SerializableMap.union union issue_handle_map
  in
  List.fold callables ~f:accumulate_issue_handles ~init:IssueHandle.SerializableMap.empty


(* Return a map from main issues (which will be attached with the secondary issues) to secondary
   issues. *)
let collect_main_issues ~taint_configuration ~issue_handle_map =
  let open Issue in
  let is_main_issue issue = MultiSource.is_main_issue ~taint_configuration issue in
  let accumulate ~sink ~related_issues ~issue so_far =
    if List.is_empty related_issues then
      (* The related issues along with the current issue do not consitute a complete set of issues,
         in order for filing a multi-source issue. *)
      so_far
    else
      let all_issues = issue :: related_issues in
      let main_issues, secondary_issues = List.partition_tf ~f:(is_main_issue ~sink) all_issues in
      if List.is_empty main_issues then
        failwith "Expected at least one main issue."
      else
        List.cartesian_product main_issues secondary_issues
        |> List.fold ~init:so_far ~f:(fun so_far (main_issue, secondary_issue) ->
               IssueHandle.SerializableMap.add_multi
                 ~key:main_issue.handle
                 ~data:secondary_issue.handle
                 so_far)
  in
  let accumulate_per_issue so_far issue =
    Sinks.Map.fold
      (fun sink related_issues so_far -> accumulate ~sink ~related_issues ~issue so_far)
      (MultiSource.find_related_issues ~taint_configuration ~issue_handle_map issue)
      so_far
  in
  IssueHandle.SerializableMap.data issue_handle_map
  |> List.fold ~init:IssueHandle.SerializableMap.empty ~f:accumulate_per_issue


(* For a multi-source rule, only keep its main issue, based on the taint configuration.
   Additionally, we attach the main issues with the secondary issues. This function updates the
   analysis results that are stored in `fixpoint_state`. *)
let update_multi_source_issues ~filename_lookup ~taint_configuration ~callables ~fixpoint_state =
  let issue_handle_map = issue_handle_map ~callables ~fixpoint_state in
  let main_issues = collect_main_issues ~taint_configuration ~issue_handle_map in
  let attach_secondary_issue ~main_issue_location issue_so_far secondary_issue_handle =
    let secondary_issue =
      IssueHandle.SerializableMap.find secondary_issue_handle issue_handle_map
    in
    let source_traces =
      Issue.MultiSource.get_first_source_hops ~main_issue_location ~filename_lookup secondary_issue
    in
    let sink_traces =
      Issue.MultiSource.get_first_sink_hops ~main_issue_location ~filename_lookup secondary_issue
    in
    Issue.MultiSource.attach_extra_traces ~source_traces ~sink_traces issue_so_far
  in
  let update_multi_source_issue issue =
    if not (Issue.MultiSource.is_multi_source issue) then
      Some issue
    else
      match IssueHandle.SerializableMap.find_opt issue.handle main_issues with
      | Some secondary_issue_handles ->
          (* This is a main issue that needs to be attached with a secondary issue. *)
          let main_issue_location = Issue.canonical_location issue in
          Some
            (List.fold
               secondary_issue_handles
               ~init:issue
               ~f:(attach_secondary_issue ~main_issue_location))
      | None -> (* This is a secondary issue that needs to be deleted. *) None
  in
  let issues =
    IssueHandle.SerializableMap.data issue_handle_map
    |> List.filter_map ~f:update_multi_source_issue
  in
  let callables_to_issues =
    List.fold issues ~init:Target.Map.empty ~f:(fun so_far issue ->
        Target.Map.add_multi ~key:issue.Issue.handle.callable ~data:issue so_far)
  in
  let update_issues_for_callable (callable, issues) =
    issues
    |> List.fold ~init:[] ~f:(fun so_far issue -> (issue.Issue.handle, issue) :: so_far)
    |> IssueHandle.SerializableMap.of_alist_exn
    |> Fixpoint.set_result fixpoint_state callable
  in
  Fixpoint.clear_results fixpoint_state;
  List.iter (Target.Map.to_alist callables_to_issues) ~f:update_issues_for_callable
