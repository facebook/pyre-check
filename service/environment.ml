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
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    sources
  =
  let resolution = Environment.resolution environment () in
  let populate () =
    List.iter sources ~f:(Environment.register_module environment);
    sources
    |> List.map ~f:(fun { Ast.Source.qualifier; _ } -> qualifier)
    |> List.iter ~f:(Environment.register_implicit_submodules environment);
    let all_annotations =
      List.fold
        ~init:Environment.built_in_annotations
        ~f:(fun annotations source ->
          Set.union annotations (Environment.register_class_definitions environment source))
        sources
      |> Set.to_list
    in
    Environment.register_aliases environment sources;
    List.iter ~f:(Environment.register_dependencies environment) sources;

    (* Build type order. *)
    List.iter ~f:(Environment.connect_type_order environment resolution) sources;
    Environment.deduplicate_class_hierarchy ~annotations:all_annotations;
    if debug then
      (* Validate integrity of the type order built so far before moving forward. Further
         transformations might be incorrect or not terminate otherwise. *)
      Environment.check_class_hierarchy_integrity ();
    Environment.connect_annotations_to_object all_annotations;
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
  Environment.transaction
    environment
    ~f:(fun () -> List.iter ~f:(Plugin.apply_to_environment environment resolution) sources)
    ();

  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.AttributeCache.clear ()


let build environment ~configuration ~scheduler sources =
  Log.info "Building type environment...";

  (* This grabs all sources from shared memory. It is unavoidable: Environment must be built
     sequentially until we find a way to build the environment in parallel. *)
  let timer = Timer.start () in
  populate ~configuration ~scheduler environment sources;
  Statistics.performance ~name:"full environment built" ~timer ();
  if Log.is_enabled `Dotty then (
    let type_order_file =
      Path.create_relative
        ~root:(Configuration.Analysis.pyre_root configuration)
        ~relative:"type_order.dot"
    in
    Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
    File.create ~content:(Environment.class_hierarchy_dot ()) type_order_file |> File.write )


(** First dumps environment to shared memory, then exposes through Environment_handler *)
let populate_shared_memory
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    ~ast_environment
    sources
  =
  Log.info "Adding built-in environment information to shared memory...";
  let timer = Timer.start () in
  let shared_handler = Environment.shared_memory_handler ast_environment in
  Environment.fill_shared_memory_with_default_typeorder ();
  Environment.add_special_classes shared_handler;
  Environment.add_dummy_modules shared_handler;
  Environment.add_special_globals shared_handler;
  Statistics.performance ~name:"added environment to shared memory" ~timer ();
  build shared_handler ~configuration ~scheduler sources;
  if debug then (
    Environment.check_class_hierarchy_integrity ();
    Statistics.event
      ~section:`Memory
      ~name:"shared memory size"
      ~integers:["size", Memory.heap_size ()]
      () );
  shared_handler
