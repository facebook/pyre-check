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


let build environment ~configuration ~scheduler ~ast_environment sources =
  Log.info "Building type environment...";

  (* This grabs all sources from shared memory. It is unavoidable: Environment must be built
     sequentially until we find a way to build the environment in parallel. *)
  let timer = Timer.start () in
  let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
  let qualifiers = List.map sources ~f:(fun { Ast.Source.qualifier; _ } -> qualifier) in
  let update_result =
    UnannotatedGlobalEnvironment.update
      unannotated_global_environment
      ~scheduler
      ~configuration
      (Ast.Reference.Set.of_list qualifiers)
  in
  populate
    ~configuration
    ~scheduler
    ~update_result
    environment
    (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    sources;
  Statistics.performance ~name:"full environment built" ~timer ();
  if Log.is_enabled `Dotty then (
    let type_order_file =
      Path.create_relative
        ~root:(Configuration.Analysis.pyre_root configuration)
        ~relative:"type_order.dot"
    in
    Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
    File.create ~content:(Environment.class_hierarchy_dot environment) type_order_file
    |> File.write )


(** First dumps environment to shared memory, then exposes through Environment_handler *)
let populate_shared_memory
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    ~ast_environment
    sources
  =
  Log.info "Adding built-in environment information to shared memory...";
  let timer = Timer.start () in
  let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
  let shared_handler =
    Environment.shared_memory_handler
      (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
  in
  Environment.fill_shared_memory_with_default_typeorder shared_handler;
  Environment.add_dummy_modules shared_handler;
  Environment.add_special_globals shared_handler;
  Statistics.performance ~name:"added environment to shared memory" ~timer ();
  build shared_handler ~configuration ~scheduler ~ast_environment sources;
  if debug then (
    Environment.check_class_hierarchy_integrity shared_handler;
    Statistics.event
      ~section:`Memory
      ~name:"shared memory size"
      ~integers:["size", Memory.heap_size ()]
      () );
  shared_handler
