(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module CoveredRule = struct
  module T = struct
    type t = {
      rule_code: int;
      (* Evidence about why the rule is considered as being "covered". *)
      kind_coverage: KindCoverage.t;
    }
    [@@deriving eq, show, compare, sexp, hash]
  end

  include T

  let is_covered ~kind_coverage ({ Rule.code; _ } as rule) =
    let ({ KindCoverage.sources = _; sinks = _; transforms = transforms_from_rule } as from_rule) =
      KindCoverage.from_rule rule
    in
    let ({
           KindCoverage.sources = intersected_sources;
           sinks = intersected_sinks;
           transforms = intersected_transforms;
         } as intersected)
      =
      KindCoverage.intersect from_rule kind_coverage
    in
    let open KindCoverage in
    if
      Sources.Set.is_empty intersected_sources
      || Sinks.Set.is_empty intersected_sinks
      || (not (Transforms.Set.is_empty transforms_from_rule))
         && Transforms.Set.is_empty intersected_transforms
    then
      None
    else
      Some { rule_code = code; kind_coverage = intersected }


  module Set = Data_structures.SerializableSet.Make (T)
end

module IntSet = Data_structures.SerializableSet.Make (struct
  type t = Int.t [@@deriving compare, hash, show, sexp]
end)

type t = {
  covered_rules: CoveredRule.Set.t;
  uncovered_rule_codes: IntSet.t;
}
[@@deriving eq, show]

let from_rules ~kind_coverage rules =
  let module RuleMap = Stdlib.Map.Make (Int) in
  (* Group rules by code. *)
  let rule_map =
    let update_rules ~rule = function
      | Some existing_rules -> Some (rule :: existing_rules)
      | None -> Some [rule]
    in
    List.fold rules ~init:RuleMap.empty ~f:(fun rule_map ({ Rule.code; _ } as rule) ->
        RuleMap.update code (update_rules ~rule) rule_map)
  in
  let covered_rules, uncovered_rule_codes =
    RuleMap.fold
      (fun code rules (covered_rules, uncovered_rule_codes) ->
        match rules with
        | [rule] -> (
            match CoveredRule.is_covered ~kind_coverage rule with
            | Some covered_rule ->
                CoveredRule.Set.add covered_rule covered_rules, uncovered_rule_codes
            | None -> covered_rules, IntSet.add code uncovered_rule_codes)
        | [rule_1; rule_2] -> (
            (* A multi-source rule is covered, only if both sub-rules are covered. *)
            match
              ( CoveredRule.is_covered ~kind_coverage rule_1,
                CoveredRule.is_covered ~kind_coverage rule_2 )
            with
            | ( Some { CoveredRule.kind_coverage = kind_coverage_1; _ },
                Some { CoveredRule.kind_coverage = kind_coverage_2; _ } ) ->
                let covered_rule =
                  {
                    CoveredRule.rule_code = code;
                    kind_coverage = KindCoverage.union kind_coverage_1 kind_coverage_2;
                  }
                in
                CoveredRule.Set.add covered_rule covered_rules, uncovered_rule_codes
            | _ -> covered_rules, IntSet.add code uncovered_rule_codes)
        | _ ->
            failwith
              "Expect exactly one or two two rules per rule code when computing category coverage")
      rule_map
      (CoveredRule.Set.empty, IntSet.empty)
  in
  { covered_rules; uncovered_rule_codes }
