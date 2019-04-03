(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module ServiceTypeOrder = TypeOrder
open Analysis
open Ast
open Pyre

open EnvironmentSharedMemory
open PostprocessSharedMemory


let populate
    (module Handler: Environment.Handler)
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    sources =
  let resolution = TypeCheck.resolution (module Handler) () in
  let populate () =
    List.iter ~f:(Environment.register_module (module Handler)) sources;

    let all_annotations =
      List.fold
        ~init:Environment.built_in_annotations
        ~f:(fun annotations source ->
            Set.union annotations (Environment.register_class_definitions (module Handler) source))
        sources
      |> Set.to_list
    in
    Environment.register_aliases (module Handler) sources;

    List.iter
      ~f:(Environment.register_dependencies (module Handler))
      sources;
    (* Build type order. *)
    List.iter ~f:(Environment.connect_type_order (module Handler) resolution) sources;
    TypeOrder.deduplicate (module Handler.TypeOrderHandler) ~annotations:all_annotations;

    if debug then
      (* Validate integrity of the type order built so far before moving forward.
         Further transformations might be incorrect or not terminate otherwise. *)
      TypeOrder.check_integrity (module Handler.TypeOrderHandler);

    TypeOrder.connect_annotations_to_top
      (module Handler.TypeOrderHandler)
      ~top:Type.object_primitive
      all_annotations;
    TypeOrder.remove_extra_edges
      (module Handler.TypeOrderHandler)
      ~bottom:Type.Bottom
      ~top:Type.object_primitive
      all_annotations;
    Type.Cache.disable ();
    (* TODO(T30713406): Merge with class registration. *)
    List.iter ~f:Handler.refine_class_definition all_annotations;
    Type.Cache.enable ();
    List.iter ~f:(Environment.propagate_nested_classes (module Handler) resolution) all_annotations
  in
  Handler.transaction ~f:populate ();
  Scheduler.iter
    scheduler
    ~configuration
    ~f:(fun sources ->
        List.iter
          sources
          ~f:(Environment.register_values (module Handler) resolution))
    ~inputs:sources;
  Handler.transaction
    ~f:(fun () -> List.iter ~f:(Plugin.apply_to_environment (module Handler) resolution) sources)
    ();
  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.Attribute.Cache.clear ()


let build
    ((module Handler: Environment.Handler) as handler)
    ~configuration
    ~scheduler
    ~stubs
    ~sources =
  Log.info "Building type environment...";
  (* This grabs all sources from shared memory. It is unavoidable: Environment
     must be built sequentially until we find a way to build the environment in
     parallel. *)
  let get_sources =
    List.fold
      ~init:[]
      ~f:(fun handles handle ->
          match Ast.SharedMemory.Sources.get handle with
          | Some handle -> handle :: handles
          | None -> handles)
  in
  let timer = Timer.start () in
  let stubs = get_sources stubs in
  List.iter ~f:(Environment.register_module (module Handler)) stubs;
  let sources =
    (* If a stub matching a handle's qualifier already exists, we shouldn't override. *)
    let should_keep { Source.handle; qualifier; _ } =
      Handler.module_definition qualifier
      >>= Module.handle
      >>| File.Handle.equal handle
      |> Option.value ~default:true
    in
    let sources = get_sources sources in
    List.filter ~f:should_keep sources
  in
  populate ~configuration ~scheduler handler (stubs @ sources);
  Statistics.performance ~name:"full environment built" ~timer ();

  if Log.is_enabled `Dotty then
    begin
      let type_order_file =
        Path.create_relative
          ~root:(Configuration.Analysis.pyre_root configuration)
          ~relative:"type_order.dot"
      in
      let (module Handler: Environment.Handler) = handler in
      Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
      File.create
        ~content:(TypeOrder.to_dot (module Handler.TypeOrderHandler))
        type_order_file
      |> File.write
    end



module SharedHandler: Analysis.Environment.Handler = struct
  let transaction ~f () =
    Ast.SharedMemory.Modules.begin_transaction ();

    Protocols.LocalChanges.push_stack ();
    FunctionKeys.LocalChanges.push_stack ();
    ClassKeys.LocalChanges.push_stack ();
    AliasKeys.LocalChanges.push_stack ();
    GlobalKeys.LocalChanges.push_stack ();
    DependentKeys.LocalChanges.push_stack ();
    Dependents.LocalChanges.push_stack ();
    ClassDefinitions.LocalChanges.push_stack ();
    Globals.LocalChanges.push_stack ();
    Aliases.LocalChanges.push_stack ();
    OrderEdges.LocalChanges.push_stack ();
    OrderBackedges.LocalChanges.push_stack ();
    OrderAnnotations.LocalChanges.push_stack ();
    OrderKeys.LocalChanges.push_stack ();
    OrderIndices.LocalChanges.push_stack ();

    let result = f () in

    Ast.SharedMemory.Modules.end_transaction ();

    Protocols.LocalChanges.commit_all ();
    FunctionKeys.LocalChanges.commit_all ();
    ClassKeys.LocalChanges.commit_all ();
    AliasKeys.LocalChanges.commit_all ();
    GlobalKeys.LocalChanges.commit_all ();
    DependentKeys.LocalChanges.commit_all ();
    Dependents.LocalChanges.commit_all ();
    ClassDefinitions.LocalChanges.commit_all ();
    Globals.LocalChanges.commit_all ();
    Aliases.LocalChanges.commit_all ();
    OrderEdges.LocalChanges.commit_all ();
    OrderBackedges.LocalChanges.commit_all ();
    OrderAnnotations.LocalChanges.commit_all ();
    OrderKeys.LocalChanges.commit_all ();
    OrderIndices.LocalChanges.commit_all ();

    Protocols.LocalChanges.pop_stack ();
    FunctionKeys.LocalChanges.pop_stack ();
    ClassKeys.LocalChanges.pop_stack ();
    AliasKeys.LocalChanges.pop_stack ();
    GlobalKeys.LocalChanges.pop_stack ();
    DependentKeys.LocalChanges.pop_stack ();
    Dependents.LocalChanges.pop_stack ();
    ClassDefinitions.LocalChanges.pop_stack ();
    Globals.LocalChanges.pop_stack ();
    Aliases.LocalChanges.pop_stack ();
    OrderEdges.LocalChanges.pop_stack ();
    OrderBackedges.LocalChanges.pop_stack ();
    OrderAnnotations.LocalChanges.pop_stack ();
    OrderKeys.LocalChanges.pop_stack ();
    OrderIndices.LocalChanges.pop_stack ();
    result


  let class_definition =
    ClassDefinitions.get

  let register_protocol protocol =
    let protocols = Protocols.get 0 |> Option.value ~default:[] in
    Protocols.add 0 (protocol :: protocols)


  let protocols () =
    Protocols.get 0
    |> Option.value ~default:[]

  let register_module ~qualifier ~local_mode ~handle ~stub ~statements =
    let string =
      Annotation.create_immutable ~global:true Type.string
      |> Node.create_with_default_location
    in
    let global_key = Reference.create ~prefix:qualifier in
    Globals.write_through (global_key "__file__") string;
    Globals.write_through (global_key "__name__") string;
    let dictionary_annotation =
      Type.dictionary ~key:Type.string ~value:Type.Any
      |> Annotation.create_immutable ~global:true
      |> Node.create_with_default_location
    in
    Globals.write_through (global_key "__dict__") dictionary_annotation;

    let is_registered_empty_stub =
      Ast.SharedMemory.Modules.get ~qualifier
      >>| Module.empty_stub
      |> Option.value ~default:false
    in
    if not is_registered_empty_stub then
      begin
        Ast.SharedMemory.Modules.add
          ~qualifier
          ~ast_module:
            (Module.create
               ~qualifier
               ~local_mode
               ?handle
               ~stub
               statements)
      end

  let is_module qualifier =
    Ast.SharedMemory.Modules.exists ~qualifier

  let module_definition qualifier =
    Ast.SharedMemory.Modules.get ~qualifier

  let in_class_definition_keys annotation =
    ClassDefinitions.mem annotation

  let aliases =
    Aliases.get

  let globals =
    Globals.get

  let dependencies =
    Dependents.get

  module DependencyHandler = (struct
    let add_new_key ~get ~add ~handle ~key =
      let existing = get handle in
      match existing with
      | None -> add handle [key]
      | Some keys -> add handle (key :: keys)


    let add_function_key ~handle reference =
      add_new_key
        ~handle
        ~key:reference
        ~get:FunctionKeys.get
        ~add:FunctionKeys.add

    let add_class_key ~handle class_type =
      add_new_key
        ~handle
        ~key:class_type
        ~get:ClassKeys.get
        ~add:ClassKeys.add

    let add_alias_key ~handle alias =
      add_new_key
        ~handle
        ~key:alias
        ~get:AliasKeys.get
        ~add:AliasKeys.add

    let add_global_key ~handle global =
      add_new_key
        ~handle
        ~key:global
        ~get:GlobalKeys.get
        ~add:GlobalKeys.add

    let add_dependent_key ~handle dependent =
      add_new_key
        ~handle
        ~key:dependent
        ~get:DependentKeys.get
        ~add:DependentKeys.add

    let add_dependent ~handle dependent =
      add_dependent_key ~handle dependent;
      match Dependents.get dependent with
      | None -> Dependents.add dependent (File.Handle.Set.Tree.singleton handle)
      | Some dependencies -> Dependents.add dependent (File.Handle.Set.Tree.add dependencies handle)

    let get_function_keys ~handle = FunctionKeys.get handle |> Option.value ~default:[]
    let get_class_keys ~handle = ClassKeys.get handle |> Option.value ~default:[]
    let get_alias_keys ~handle = AliasKeys.get handle |> Option.value ~default:[]
    let get_global_keys ~handle = GlobalKeys.get handle |> Option.value ~default:[]
    let get_dependent_keys ~handle = DependentKeys.get handle |> Option.value ~default:[]

    let clear_keys_batch handles =
      FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list handles);
      ClassKeys.remove_batch (ClassKeys.KeySet.of_list handles);
      AliasKeys.remove_batch (AliasKeys.KeySet.of_list handles);
      GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list handles);
      DependentKeys.remove_batch (DependentKeys.KeySet.of_list handles)

    let dependents = Dependents.get

    let normalize handles =
      let normalize_keys handle =
        begin
          match FunctionKeys.get handle with
          | Some keys ->
              FunctionKeys.remove_batch (FunctionKeys.KeySet.singleton handle);
              FunctionKeys.add handle (List.dedup_and_sort ~compare:Reference.compare keys)
          | None ->
              ()
        end;
        begin
          match ClassKeys.get handle with
          | Some keys ->
              ClassKeys.remove_batch (ClassKeys.KeySet.singleton handle);
              ClassKeys.add handle (List.dedup_and_sort ~compare:Type.compare keys)
          | None ->
              ()
        end;
        begin
          match AliasKeys.get handle with
          | Some keys ->
              AliasKeys.remove_batch (AliasKeys.KeySet.singleton handle);
              AliasKeys.add handle (List.dedup_and_sort ~compare:Type.compare keys)
          | None ->
              ()
        end;
        begin
          match GlobalKeys.get handle with
          | Some keys ->
              GlobalKeys.remove_batch (GlobalKeys.KeySet.singleton handle);
              GlobalKeys.add handle (List.dedup_and_sort ~compare:Reference.compare keys)
          | None ->
              ()
        end;
        begin
          match DependentKeys.get handle with
          | Some keys ->
              DependentKeys.remove_batch (DependentKeys.KeySet.singleton handle);
              DependentKeys.add handle (List.dedup_and_sort ~compare:Reference.compare keys)
          | None ->
              ()
        end
      in
      List.iter handles ~f:normalize_keys;
      let normalize_dependents name =
        match Dependents.get name with
        | Some unnormalized ->
            Dependents.remove_batch (Dependents.KeySet.singleton name);
            File.Handle.Set.Tree.to_list unnormalized
            |> List.sort ~compare:File.Handle.compare
            |> File.Handle.Set.Tree.of_list
            |> Dependents.add name
        | None ->
            ()
      in
      List.concat_map handles ~f:(fun handle -> get_dependent_keys ~handle)
      |> List.dedup_and_sort ~compare:Reference.compare
      |> List.iter ~f:normalize_dependents
  end: Dependencies.Handler)
  module TypeOrderHandler = ServiceTypeOrder.Handler
  let refine_class_definition annotation =
    let open Statement in
    let refine
        { Resolution.class_definition = { Node.location; value = class_definition }; _ } =
      let successors = TypeOrder.successors (module TypeOrderHandler) annotation in
      let in_test =
        let is_unit_test { Resolution.class_definition = { Node.value = definition; _ }; _ } =
          Class.is_unit_test definition
        in
        let successor_classes =
          successors
          |> List.filter_map ~f:ClassDefinitions.get
        in
        List.exists ~f:is_unit_test successor_classes
      in
      let explicit_attributes = Class.explicitly_assigned_attributes class_definition in
      let implicit_attributes = Class.implicit_attributes ~in_test class_definition in
      ClassDefinitions.write_through
        annotation
        {
          class_definition = { Node.location; value = class_definition };
          is_test = in_test;
          successors;
          explicit_attributes;
          implicit_attributes;
          methods = [];
        };
    in
    ClassDefinitions.get annotation
    >>| refine
    |> ignore

  let register_dependency ~handle ~dependency =
    Log.log
      ~section:`Dependencies
      "Adding dependency from %a to %a"
      Reference.pp dependency
      File.Handle.pp handle;
    DependencyHandler.add_dependent ~handle dependency


  let register_global ~handle ~reference ~global =
    DependencyHandler.add_global_key ~handle reference;
    Globals.write_through reference global


  let set_class_definition ~primitive ~definition =
    let definition =
      match ClassDefinitions.get primitive with
      | Some ({
          Resolution.class_definition = { Node.location; value = preexisting };
          _;
        } as representation) ->
          {
            representation with
            Resolution.class_definition = {
              Node.location;
              value = Statement.Class.update preexisting ~definition:(Node.value definition);
            };
          }
      | _ ->
          {
            Resolution.class_definition = definition;
            methods = [];
            successors = [];
            explicit_attributes = Identifier.SerializableMap.empty;
            implicit_attributes = Identifier.SerializableMap.empty;
            is_test = false;
          }
    in
    ClassDefinitions.add primitive definition

  let register_alias ~handle ~key ~data =
    DependencyHandler.add_alias_key ~handle key;
    Aliases.add key data

  let purge ?(debug = false) handles =
    let purge_dependents keys =
      let remove_handle dependents =
        File.Handle.Set.Tree.filter
          ~f:(fun dependent -> not (List.mem handles dependent ~equal:File.Handle.equal))
          dependents
      in
      List.iter
        ~f:(fun key -> Dependents.get key >>| remove_handle >>| Dependents.add key |> ignore)
        keys;
      DependentKeys.remove_batch (DependentKeys.KeySet.of_list handles)
    in
    List.concat_map ~f:(fun handle -> DependencyHandler.get_function_keys ~handle) handles
    |> fun keys ->
    begin
      (* We add a global name for each function definition as well. *)
      Globals.remove_batch (Globals.KeySet.of_list keys);
    end;

    (* Remove the connection to the parent (if any) for all
       classes defined in the updated handles. *)
    List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
    |> List.iter ~f:(TypeOrder.disconnect_successors (module TypeOrderHandler));

    List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
    |> fun keys -> ClassDefinitions.remove_batch (ClassDefinitions.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_alias_keys ~handle) handles
    |> fun keys -> Aliases.remove_batch (Aliases.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_global_keys ~handle) handles
    |> fun keys -> Globals.remove_batch (Globals.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_dependent_keys ~handle) handles
    |> List.dedup_and_sort ~compare:Reference.compare
    |> purge_dependents;

    DependencyHandler.clear_keys_batch handles;
    List.map ~f:(fun handle -> Ast.Source.qualifier ~handle) handles
    |> fun qualifiers -> Ast.SharedMemory.Modules.remove ~qualifiers;

    if debug then
      (* If in debug mode, make sure the TypeOrder is still consistent. *)
      TypeOrder.check_integrity (module TypeOrderHandler)

  let local_mode handle = ErrorModes.get handle
end


(** First dumps environment to shared memory, then exposes through
    Environment_handler *)
let populate_shared_memory
    ~configuration:({ Configuration.Analysis.debug; _ } as configuration)
    ~scheduler
    ~stubs
    ~sources =
  let add_to_shared_memory
      {
        Environment.class_definitions;
        protocols;
        modules;
        aliases;
        globals;
        order;
        dependencies = {
          Dependencies.index = {
            Dependencies.function_keys;
            class_keys;
            alias_keys;
            global_keys;
            dependent_keys;
          };
          dependents;
        };
      } =
    Log.info "Adding environment information to shared memory...";
    (* Writing through the caches because we are doing a
       batch-add. Especially while still adding amounts of data that
       exceed the cache size, the time spent doing cache bookkeeping
       is wasted. *)
    let timer = Timer.start () in
    let add_table f = Hashtbl.iteri ~f:(fun ~key ~data -> f key data) in
    let add_type_order { TypeOrder.edges; backedges; indices; annotations } =
      add_table OrderEdges.write_through edges;
      let add_backedge key targets =
        let targets = TypeOrder.Target.Set.of_list targets in
        OrderBackedges.write_through key (Set.to_list targets)
      in
      add_table add_backedge backedges;

      add_table OrderBackedges.write_through backedges;

      add_table OrderIndices.write_through indices;
      add_table OrderAnnotations.write_through annotations;
      OrderKeys.write_through "Order" (Hashtbl.keys annotations);
    in
    add_type_order order;

    add_table ClassDefinitions.write_through class_definitions;
    add_table Aliases.add aliases;
    add_table Globals.write_through globals;
    add_table Dependents.write_through (Hashtbl.map ~f:Set.to_tree dependents);
    add_table FunctionKeys.write_through (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.write_through (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.write_through (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.write_through (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.write_through (Hashtbl.map ~f:Hash_set.to_list dependent_keys);

    Protocols.write_through 0 (Hash_set.to_list protocols);
    add_table
      (fun qualifier ast_module -> Ast.SharedMemory.Modules.add ~qualifier ~ast_module)
      modules;
    Statistics.performance ~name:"added environment to shared memory" ~timer ()
  in
  let environment = Environment.Builder.create () in
  add_to_shared_memory environment;
  build (module SharedHandler) ~configuration ~scheduler ~stubs ~sources;

  let resolution = TypeCheck.resolution (module SharedHandler) () in
  Environment.infer_protocols ~handler:(module SharedHandler) resolution ();

  if debug then
    TypeOrder.check_integrity (module SharedHandler.TypeOrderHandler);
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", EnvironmentSharedMemory.heap_size ()]
    ()


let normalize_shared_memory () =
  TypeOrder.normalize (module SharedHandler.TypeOrderHandler);
  (* Since we don't provide an API to the raw order keys in the type order handler,
     handle it inline here. *)
  begin
    match OrderKeys.get "Order" with
    | None ->
        ()
    | Some keys ->
        OrderKeys.remove_batch (OrderKeys.KeySet.singleton "Order");
        List.sort ~compare:Int.compare keys
        |> OrderKeys.add "Order";
  end;
  Ast.SharedMemory.HandleKeys.normalize ();
  let handles = Ast.SharedMemory.HandleKeys.get () in
  File.Handle.Set.Tree.to_list handles
  |> SharedHandler.DependencyHandler.normalize
