(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServiceTypeOrder = TypeOrder
open Analysis

let populate
    environment
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    ~update_result
    _
  =
  let validate_hierarchy () =
    (* Validate integrity of the type order built so far before moving forward. Further
       transformations might be incorrect or not terminate otherwise. *)
    let resolution = Environment.resolution environment () in
    let indices =
      Environment.unannotated_global_environment environment
      |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
    in
    GlobalResolution.class_hierarchy resolution |> ClassHierarchy.check_integrity ~indices
  in
  if debug then
    validate_hierarchy ();
  Environment.update_and_compute_dependencies environment ~scheduler ~configuration update_result
  |> ignore;

  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.AttributeCache.clear ()
