(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* A list of non-overlapping intervals sorted with the lower bounds of intervals *)
type t = ClassInterval.t list

(* For example, (1,2);(3,4) becomes (1,4) *)
let remove_gaps_between_nonoverlapping_intervals list =
  let rec remove_gaps list result =
    match list with
    | first_interval :: second_interval :: tail ->
        let first_lower = ClassInterval.lower_bound_exn first_interval in
        let first_upper = ClassInterval.upper_bound_exn first_interval in
        let second_lower = ClassInterval.lower_bound_exn second_interval in
        let second_upper = ClassInterval.upper_bound_exn second_interval in
        if first_upper + 1 == second_lower then
          remove_gaps (ClassInterval.create first_lower second_upper :: tail) result
        else
          remove_gaps (second_interval :: tail) (first_interval :: result)
    | [element] -> element :: result
    | [] -> result
  in
  remove_gaps list [] |> List.rev


let sort_list list =
  let list = List.filter list ~f:(fun interval -> not (ClassInterval.is_empty interval)) in
  let list =
    List.sort list ~compare:(fun left_interval right_interval ->
        Int.compare
          (ClassInterval.lower_bound_exn left_interval)
          (ClassInterval.lower_bound_exn right_interval))
  in
  let rec merge_overlapping_intervals list result =
    match list with
    | first_interval :: second_interval :: tail ->
        let first_lower = ClassInterval.lower_bound_exn first_interval in
        let first_upper = ClassInterval.upper_bound_exn first_interval in
        let second_lower = ClassInterval.lower_bound_exn second_interval in
        let second_upper = ClassInterval.upper_bound_exn second_interval in
        if second_lower > first_upper then
          let result = first_interval :: result in
          merge_overlapping_intervals (second_interval :: tail) result
        else if second_upper <= first_upper then
          merge_overlapping_intervals (first_interval :: tail) result
        else
          merge_overlapping_intervals (ClassInterval.create first_lower second_upper :: tail) result
    | element :: _ -> element :: result
    | [] -> result
  in
  merge_overlapping_intervals list [] |> List.rev |> remove_gaps_between_nonoverlapping_intervals


let of_list = sort_list

let to_list intervals = intervals

let pp formatter intervals =
  List.iter intervals ~f:(fun interval ->
      Format.fprintf formatter "@[(%a);@]" ClassInterval.pp_interval interval)


let show = Format.asprintf "%a" pp

let show_list = show

let equal left right = List.equal ClassInterval.equal left right

let empty = []

let bottom = empty

let top = [ClassInterval.top]

let is_empty = List.is_empty

let is_top = function
  | [head] -> ClassInterval.is_top head
  | _ -> false


let lower_bound_exn intervals = List.hd_exn intervals |> ClassInterval.lower_bound_exn

let upper_bound_exn intervals = List.last_exn intervals |> ClassInterval.upper_bound_exn

let meet left right =
  let rec intersect_interval_lists ~left ~right ~result =
    match left, right with
    | left_head :: left_tail, right_head :: right_tail ->
        let left_lower = ClassInterval.lower_bound_exn left_head in
        let left_upper = ClassInterval.upper_bound_exn left_head in
        let right_lower = ClassInterval.lower_bound_exn right_head in
        let right_upper = ClassInterval.upper_bound_exn right_head in
        if left_lower <= right_lower then
          if left_upper >= right_upper then (* l1[p1] covers l2[p2] *)
            intersect_interval_lists ~left ~right:right_tail ~result:(right_head :: result)
          else if (* l2[p2] ends later *) left_upper < right_lower then (* No overlap *)
            intersect_interval_lists ~left:left_tail ~right ~result
          else (* l2[p2] ends later *)
            intersect_interval_lists
              ~left:left_tail
              ~right
              ~result:(ClassInterval.meet left_head right_head :: result)
        else if left_upper <= right_upper then (* l2[p2] covers l1[p1] *)
          intersect_interval_lists ~left:left_tail ~right ~result:(left_head :: result)
        else if (* l1[p1] ends later *) right_upper < left_lower then (* No overlap *)
          intersect_interval_lists ~left ~right:right_tail ~result
        else (* l1[p1] ends later *)
          intersect_interval_lists
            ~left
            ~right:right_tail
            ~result:(ClassInterval.meet left_head right_head :: result)
    | _ -> result
  in
  intersect_interval_lists ~left ~right ~result:[] |> List.rev


let less_or_equal ~left ~right = equal (meet left right) left

let join left right =
  let rec merge_interval_lists ~left ~right ~result =
    match left, right with
    | left_head :: left_tail, right_head :: right_tail ->
        let left_lower = ClassInterval.lower_bound_exn left_head in
        let left_upper = ClassInterval.upper_bound_exn left_head in
        let right_lower = ClassInterval.lower_bound_exn right_head in
        let right_upper = ClassInterval.upper_bound_exn right_head in
        if left_lower <= right_lower then
          if left_upper >= right_upper (* l1[p1] covers l2[p2] *) then
            merge_interval_lists ~left ~right:right_tail ~result
          else if left_upper < right_lower (* No overlap *) then
            merge_interval_lists ~left:left_tail ~right ~result:(left_head :: result)
          else (* l1[p1] and l2[p2] intersects *)
            let new_interval = ClassInterval.create left_lower right_upper in
            merge_interval_lists ~left:left_tail ~right:(new_interval :: right_tail) ~result
        else if left_upper <= right_upper (* l2[p2] covers l1[p1] *) then
          merge_interval_lists ~left:left_tail ~right ~result
        else if right_upper < left_lower (* No overlap *) then
          merge_interval_lists ~left ~right:right_tail ~result:(right_head :: result)
        else (* l1[p1] and l2[p2] intersects *)
          let new_interval = ClassInterval.create right_lower left_upper in
          merge_interval_lists ~left:(new_interval :: left_tail) ~right:right_tail ~result
    | [], remaining
    | remaining, [] ->
        List.rev_append result remaining
  in
  merge_interval_lists ~left ~right ~result:[] |> remove_gaps_between_nonoverlapping_intervals
