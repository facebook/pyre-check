(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module CoveredRule : sig
  type t = {
    rule_code: int;
    kind_coverage: KindCoverage.t;
  }
  [@@deriving eq, show]

  val is_covered : kind_coverage:KindCoverage.t -> Rule.t -> t option
end
