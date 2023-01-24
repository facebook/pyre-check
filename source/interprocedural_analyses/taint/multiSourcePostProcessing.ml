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
  let accumulate issue_handle_map issue =
    (* TODO(T140234367): Fail if multiple issues map to the same issue handle. We have integration
       tests that demonstrate this problem. *)
    IssueHandle.Map.set issue_handle_map ~key:issue.Issue.handle ~data:issue
  in
  let accumulate_issue_handles issue_handle_map callable =
    let issues = Fixpoint.get_result fixpoint_state callable in
    List.fold issues ~init:issue_handle_map ~f:accumulate
  in
  List.fold callables ~f:accumulate_issue_handles ~init:IssueHandle.Map.empty


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
               IssueHandle.Map.add_multi so_far ~key:main_issue.handle ~data:secondary_issue.handle)
  in
  let accumulate_per_issue so_far issue =
    Sinks.Map.fold
      (fun sink related_issues so_far -> accumulate ~sink ~related_issues ~issue so_far)
      (MultiSource.find_related_issues ~taint_configuration ~issue_handle_map issue)
      so_far
  in
  IssueHandle.Map.data issue_handle_map
  |> List.fold ~init:IssueHandle.Map.empty ~f:accumulate_per_issue


(* For a multi-source rule, only keep its main issue, based on the taint configuration.
   Additionally, we attach the main issues with the secondary issues. This function updates the
   analysis results that are stored in `fixpoint_state`. *)
let update_multi_source_issues ~taint_configuration ~callables ~fixpoint_state =
  let issue_handle_map = issue_handle_map ~callables ~fixpoint_state in
  let main_issues = collect_main_issues ~taint_configuration ~issue_handle_map in
  let attach_secondary_issue issue_so_far secondary_issue_handle =
    let secondary_issue = IssueHandle.Map.find_exn issue_handle_map secondary_issue_handle in
    let source_traces = Issue.MultiSource.get_first_source_hops secondary_issue in
    let sink_traces = Issue.MultiSource.get_first_sink_hops secondary_issue in
    Issue.MultiSource.attach_extra_traces ~source_traces ~sink_traces issue_so_far
  in
  let update issue =
    if not (Issue.MultiSource.is_multi_source issue) then
      Some issue
    else
      match IssueHandle.Map.find main_issues issue.handle with
      | Some secondary_issue_handles ->
          (* This is a main issue that needs to be attached with a secondary issue. *)
          Some (List.fold secondary_issue_handles ~init:issue ~f:attach_secondary_issue)
      | None -> (* This is a secondary issue that needs to be deleted. *) None
  in
  List.iter
    ~f:(fun callable ->
      let issues = Fixpoint.get_result fixpoint_state callable in
      let issues = List.filter_map issues ~f:update in
      Fixpoint.set_result fixpoint_state callable issues)
    callables
