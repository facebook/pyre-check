(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The SourceCodeIncrementalApi provides and updatable, dependency-tracked
 * abstract interface around the simple read-only SourceCodeApi.
 *
 * The abstract dependency-tracked API, much like the legacy environment
 * layers, has three components:
 * - A dependency-aware ReadOnly, whose only job is to provide tracked
 *   or untracked versions of the SourceCodeApi
 * - An updatable Base representing a read-write incremental root source tree.
 * - An updatable Overlay, which includes qualifier ownership hooks.
 *)

module ReadOnly = struct
  type t = {
    get_tracked_api: dependency:SharedMemoryKeys.DependencyKey.registered -> SourceCodeApi.t;
    get_untracked_api: unit -> SourceCodeApi.t;
  }

  let create ~get_tracked_api ~get_untracked_api = { get_tracked_api; get_untracked_api }

  let controls { get_untracked_api; _ } = SourceCodeApi.controls (get_untracked_api ())

  let get_untracked_api { get_untracked_api; _ } = get_untracked_api ()

  let get_tracked_api { get_tracked_api; _ } = get_tracked_api
end

(* TODO(T175599570) Fill this out *)
module Base = struct end

(* TODO(T175599570) Fill this out *)
module Overlay = struct end
