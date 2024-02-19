(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module CoveredRule = struct
  type t = {
    rule_code: int;
    (* Evidence about why the rule is considered as being "covered". *)
    kind_coverage: KindCoverage.t;
  }
  [@@deriving eq, show]

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
end
