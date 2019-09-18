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
    ~update_result:_
    qualifiers
  =
  let resolution = Environment.resolution environment () in
  if debug then
    (* Validate integrity of the type order built so far before moving forward. Further
       transformations might be incorrect or not terminate otherwise. *)
    Environment.check_class_hierarchy_integrity environment;
  let register_undecorated_functions sources =
    let register = Environment.register_undecorated_functions environment resolution in
    List.iter sources ~f:register
  in
  Scheduler.iter scheduler ~configuration ~f:register_undecorated_functions ~inputs:qualifiers;
  let register_values sources =
    Environment.transaction
      environment
      ~only_global_keys:true
      ~f:(fun () -> Environment.register_values environment resolution sources)
      ()
  in
  Scheduler.iter
    scheduler
    ~configuration
    ~f:(fun sources -> List.iter sources ~f:register_values)
    ~inputs:qualifiers;

  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.AttributeCache.clear ()
