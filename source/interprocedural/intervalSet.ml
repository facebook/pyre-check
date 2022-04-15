(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* A list of non-overlapping intervals sorted with the lower bounds of intervals *)
type t = ClassInterval.t list

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
  (* For example, (1,2);(3,4) becomes (1,4) *)
  let rec remove_gaps_between_nonoverlapping_intervals list result =
    match list with
    | first_interval :: second_interval :: tail ->
        let first_lower = ClassInterval.lower_bound_exn first_interval in
        let first_upper = ClassInterval.upper_bound_exn first_interval in
        let second_lower = ClassInterval.lower_bound_exn second_interval in
        let second_upper = ClassInterval.upper_bound_exn second_interval in
        if first_upper + 1 == second_lower then
          remove_gaps_between_nonoverlapping_intervals
            (ClassInterval.create first_lower second_upper :: tail)
            result
        else
          remove_gaps_between_nonoverlapping_intervals
            (second_interval :: tail)
            (first_interval :: result)
    | element :: _ -> element :: result
    | [] -> result
  in
  let result = merge_overlapping_intervals list [] in
  let result = remove_gaps_between_nonoverlapping_intervals (List.rev result) [] in
  List.rev result


let of_list = sort_list

let to_list intervals = intervals

let pp formatter intervals =
  List.iter intervals ~f:(fun interval ->
      Format.fprintf formatter "@[(%a);@]" ClassInterval.pp_interval interval)


let show = Format.asprintf "%a" pp

let show_list = show

let equal left right = List.equal ClassInterval.equal left right
