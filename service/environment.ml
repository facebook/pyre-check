(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServiceTypeOrder = TypeOrder
open Analysis
open Pyre

let populate
    environment
    unannotated_global_environment
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    ~update_result
    sources
  =
  let resolution = Environment.resolution environment () in
  let populate () =
    let all_annotations =
      Set.to_list (UnannotatedGlobalEnvironment.UpdateResult.current_classes update_result)
    in
    List.iter sources ~f:(Environment.register_module environment);
    Environment.register_aliases environment sources;
    List.iter ~f:(Environment.register_dependencies environment) sources;

    (* Build type order. *)
    let connect annotation =
      UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
        unannotated_global_environment
        annotation
      >>| (fun definition -> Environment.connect_definition environment ~definition ~resolution)
      |> Option.iter ~f:Fn.id
    in
    List.iter ~f:connect all_annotations;
    Environment.deduplicate_class_hierarchy ~annotations:all_annotations;
    if debug then
      (* Validate integrity of the type order built so far before moving forward. Further
         transformations might be incorrect or not terminate otherwise. *)
      Environment.check_class_hierarchy_integrity environment;
    Environment.connect_annotations_to_object environment all_annotations;
    Environment.remove_extra_edges_to_object all_annotations;
    List.iter all_annotations ~f:(Environment.register_class_metadata environment);
    List.iter ~f:(Environment.propagate_nested_classes environment) sources
  in
  Environment.transaction environment ~f:populate ();
  let register_undecorated_functions sources =
    let register = Environment.register_undecorated_functions environment resolution in
    List.iter sources ~f:register
  in
  Scheduler.iter scheduler ~configuration ~f:register_undecorated_functions ~inputs:sources;
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
    ~inputs:sources;

  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.AttributeCache.clear ()
