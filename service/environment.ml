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
    List.iter  all_annotations
      ~f:(fun annotation ->
          match Type.primitive_name annotation with
          | Some name -> Handler.register_class_metadata name
          | _ -> ());
    Type.Cache.enable ();
    List.iter ~f:(Environment.propagate_nested_classes (module Handler) resolution) all_annotations
  in
  Handler.transaction ~f:populate ();
  let register_undecorated_functions sources =
    let register source =
      Preprocessing.convert source
      |> Environment.register_undecorated_functions (module Handler) resolution
    in
    List.iter sources ~f:register
  in
  Scheduler.iter
    scheduler
    ~configuration
    ~f:register_undecorated_functions
    ~inputs:sources;
  let register_values sources =
    EnvironmentSharedMemory.GlobalKeys.LocalChanges.push_stack ();
    Environment.register_values (module Handler) resolution sources;
    EnvironmentSharedMemory.GlobalKeys.LocalChanges.commit_all ();
    EnvironmentSharedMemory.GlobalKeys.LocalChanges.pop_stack ()
  in
  Scheduler.iter
    scheduler
    ~configuration
    ~f:(fun sources -> List.iter sources ~f:register_values)
    ~inputs:sources;
  Handler.transaction
    ~f:(fun () -> List.iter ~f:(Plugin.apply_to_environment (module Handler) resolution) sources)
    ();
  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.Attribute.Cache.clear ();
  Resolution.Cache.clear ()


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
    ProtocolKeys.LocalChanges.push_stack ();
    FunctionKeys.LocalChanges.push_stack ();
    ClassKeys.LocalChanges.push_stack ();
    AliasKeys.LocalChanges.push_stack ();
    GlobalKeys.LocalChanges.push_stack ();
    DependentKeys.LocalChanges.push_stack ();
    Dependents.LocalChanges.push_stack ();
    ClassDefinitions.LocalChanges.push_stack ();
    ClassMetadata.LocalChanges.push_stack ();
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
    ProtocolKeys.LocalChanges.commit_all ();
    FunctionKeys.LocalChanges.commit_all ();
    ClassKeys.LocalChanges.commit_all ();
    AliasKeys.LocalChanges.commit_all ();
    GlobalKeys.LocalChanges.commit_all ();
    DependentKeys.LocalChanges.commit_all ();
    Dependents.LocalChanges.commit_all ();
    ClassDefinitions.LocalChanges.commit_all ();
    ClassMetadata.LocalChanges.commit_all ();
    Globals.LocalChanges.commit_all ();
    Aliases.LocalChanges.commit_all ();
    OrderEdges.LocalChanges.commit_all ();
    OrderBackedges.LocalChanges.commit_all ();
    OrderAnnotations.LocalChanges.commit_all ();
    OrderKeys.LocalChanges.commit_all ();
    OrderIndices.LocalChanges.commit_all ();

    Protocols.LocalChanges.pop_stack ();
    ProtocolKeys.LocalChanges.pop_stack ();
    FunctionKeys.LocalChanges.pop_stack ();
    ClassKeys.LocalChanges.pop_stack ();
    AliasKeys.LocalChanges.pop_stack ();
    GlobalKeys.LocalChanges.pop_stack ();
    DependentKeys.LocalChanges.pop_stack ();
    Dependents.LocalChanges.pop_stack ();
    ClassDefinitions.LocalChanges.pop_stack ();
    ClassMetadata.LocalChanges.pop_stack ();
    Globals.LocalChanges.pop_stack ();
    Aliases.LocalChanges.pop_stack ();
    OrderEdges.LocalChanges.pop_stack ();
    OrderBackedges.LocalChanges.pop_stack ();
    OrderAnnotations.LocalChanges.pop_stack ();
    OrderKeys.LocalChanges.pop_stack ();
    OrderIndices.LocalChanges.pop_stack ();
    result


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

    let add_protocol_key ~handle protocol =
      add_new_key
        ~handle
        ~key:protocol
        ~get:ProtocolKeys.get
        ~add:ProtocolKeys.add

    let add_dependent ~handle dependent =
      add_dependent_key ~handle dependent;
      let qualifier = Source.qualifier ~handle in
      match Dependents.get dependent with
      | None -> Dependents.add dependent (Reference.Set.Tree.singleton qualifier)
      | Some dependencies ->
          Dependents.add dependent (Reference.Set.Tree.add dependencies qualifier)

    let get_function_keys ~handle = FunctionKeys.get handle |> Option.value ~default:[]
    let get_class_keys ~handle = ClassKeys.get handle |> Option.value ~default:[]
    let get_alias_keys ~handle = AliasKeys.get handle |> Option.value ~default:[]
    let get_global_keys ~handle = GlobalKeys.get handle |> Option.value ~default:[]
    let get_dependent_keys ~handle = DependentKeys.get handle |> Option.value ~default:[]
    let get_protocol_keys ~handle = ProtocolKeys.get handle |> Option.value ~default:[]

    let clear_keys_batch handles =
      FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list handles);
      ClassKeys.remove_batch (ClassKeys.KeySet.of_list handles);
      AliasKeys.remove_batch (AliasKeys.KeySet.of_list handles);
      GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list handles);
      DependentKeys.remove_batch (DependentKeys.KeySet.of_list handles);
      ProtocolKeys.remove_batch (ProtocolKeys.KeySet.of_list handles)

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
              ClassKeys.add handle (List.dedup_and_sort ~compare:Identifier.compare keys)
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
            Reference.Set.Tree.to_list unnormalized
            |> List.sort ~compare:Reference.compare
            |> Reference.Set.Tree.of_list
            |> Dependents.add name
        | None ->
            ()
      in
      List.concat_map handles ~f:(fun handle -> get_dependent_keys ~handle)
      |> List.dedup_and_sort ~compare:Reference.compare
      |> List.iter ~f:normalize_dependents
  end: Dependencies.Handler)


  let class_definition =
    ClassDefinitions.get

  let class_metadata =
    ClassMetadata.get

  let register_protocol ~handle protocol =
    DependencyHandler.add_protocol_key ~handle protocol;
    match Protocols.get SharedMemory.SingletonKey.key with
    | None ->
        Protocols.add SharedMemory.SingletonKey.key (Identifier.Set.Tree.singleton protocol)
    | Some protocols ->
        Protocols.add SharedMemory.SingletonKey.key (Identifier.Set.Tree.add protocols protocol)

  let protocols () =
    Protocols.get SharedMemory.SingletonKey.key
    |> Option.value ~default:Identifier.Set.Tree.empty
    |> Identifier.Set.Tree.to_list

  let register_module ~qualifier ~local_mode ~handle ~stub ~statements =
    let string =
      Annotation.create_immutable ~global:true Type.string
      |> Node.create_with_default_location
    in
    let global_key = Reference.create ~prefix:qualifier in
    Globals.add (global_key "__file__") string;
    Globals.add (global_key "__name__") string;
    let dictionary_annotation =
      Type.dictionary ~key:Type.string ~value:Type.Any
      |> Annotation.create_immutable ~global:true
      |> Node.create_with_default_location
    in
    Globals.add (global_key "__dict__") dictionary_annotation;

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

  let register_undecorated_function ~reference ~annotation =
    UndecoratedFunctions.add reference annotation

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

  let undecorated_signature =
    UndecoratedFunctions.get

  module TypeOrderHandler = ServiceTypeOrder.Handler
  let register_class_metadata class_name =
    let open Statement in
    let successors = TypeOrder.successors (module TypeOrderHandler) class_name in
    let in_test =
      let is_unit_test { Node.value = definition; _ } =
        Class.is_unit_test definition
      in
      let successor_classes =
        List.filter_map ~f:ClassDefinitions.get successors
      in
      List.exists ~f:is_unit_test successor_classes
    in
    ClassMetadata.add
      class_name
      {
        Resolution.is_test = in_test;
        successors;
      }


  let register_dependency ~handle ~dependency =
    Log.log
      ~section:`Dependencies
      "Adding dependency from %a to %a"
      Reference.pp dependency
      File.Handle.pp handle;
    DependencyHandler.add_dependent ~handle dependency


  let register_global ~handle ~reference ~global =
    DependencyHandler.add_global_key ~handle reference;
    Globals.add reference global


  let set_class_definition ~name ~definition =
    let definition =
      match ClassDefinitions.get name with
      | Some { Node.location; value = preexisting } ->
          {
            Node.location;
            value = Statement.Class.update preexisting ~definition:(Node.value definition);
          };
      | _ ->
          definition
    in
    ClassDefinitions.add name definition

  let register_alias ~handle ~key ~data =
    DependencyHandler.add_alias_key ~handle key;
    Aliases.add key data

  let purge ?(debug = false) handles =
    let purge_dependents keys =
      let new_dependents = Reference.Table.create () in
      let recompute_dependents key dependents =
        let qualifiers =
          handles
          |> List.map ~f:(fun handle -> Source.qualifier ~handle)
          |> Reference.Set.Tree.of_list
        in
        Hashtbl.set
          new_dependents
          ~key
          ~data:(
          Reference.Set.Tree.diff
            dependents
            qualifiers
        )
      in
      List.iter ~f:(fun key -> Dependents.get key >>| recompute_dependents key |> ignore) keys;
      Dependents.remove_batch (Dependents.KeySet.of_list (Hashtbl.keys new_dependents));
      Hashtbl.iteri new_dependents ~f:(fun ~key ~data -> Dependents.add key data);
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
    |> List.map ~f:(fun name -> Type.Primitive name)
    |> List.iter ~f:(TypeOrder.disconnect_successors (module TypeOrderHandler));

    List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
    |> fun keys -> ClassDefinitions.remove_batch (ClassDefinitions.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_alias_keys ~handle) handles
    |> fun keys -> Aliases.remove_batch (Aliases.KeySet.of_list keys);

    let global_keys =
      List.concat_map ~f:(fun handle -> DependencyHandler.get_global_keys ~handle) handles
      |> Globals.KeySet.of_list
    in
    Globals.remove_batch global_keys;
    UndecoratedFunctions.remove_batch global_keys;

    List.concat_map ~f:(fun handle -> DependencyHandler.get_dependent_keys ~handle) handles
    |> List.dedup_and_sort ~compare:Reference.compare
    |> purge_dependents;

    let purge_protocols removed_protocols =
      match Protocols.get Memory.SingletonKey.key with
      | None ->
          ()
      | Some protocols ->
          Protocols.remove_batch (Protocols.KeySet.singleton Memory.SingletonKey.key);
          Protocols.add
            Memory.SingletonKey.key
            (Identifier.Set.Tree.diff protocols removed_protocols)
    in
    List.concat_map ~f:(fun handle -> DependencyHandler.get_protocol_keys ~handle) handles
    |> Identifier.Set.Tree.of_list
    |> purge_protocols;

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
        class_metadata;
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
            protocol_keys;
          };
          dependents;
        };
        undecorated_functions;
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
      add_table OrderBackedges.write_through backedges;
      add_table OrderIndices.write_through indices;
      add_table OrderAnnotations.write_through annotations;
      OrderKeys.write_through SharedMemory.SingletonKey.key (Hashtbl.keys annotations);
    in
    add_type_order order;

    add_table ClassDefinitions.write_through class_definitions;
    add_table ClassMetadata.write_through class_metadata;
    add_table Aliases.add aliases;
    add_table Globals.write_through globals;
    add_table UndecoratedFunctions.write_through undecorated_functions;
    add_table Dependents.write_through (Hashtbl.map ~f:Set.to_tree dependents);
    add_table FunctionKeys.write_through (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.write_through (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.write_through (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.write_through (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.write_through (Hashtbl.map ~f:Hash_set.to_list dependent_keys);
    add_table ProtocolKeys.write_through (Hashtbl.map ~f:Hash_set.to_list protocol_keys);

    protocols
    |> Hash_set.to_list
    |> Identifier.Set.Tree.of_list
    |> Protocols.write_through SharedMemory.SingletonKey.key;
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
    match OrderKeys.get SharedMemory.SingletonKey.key with
    | None ->
        ()
    | Some keys ->
        OrderKeys.remove_batch (OrderKeys.KeySet.singleton SharedMemory.SingletonKey.key);
        List.sort ~compare:Int.compare keys
        |> OrderKeys.add SharedMemory.SingletonKey.key;
  end;
  begin
    match Protocols.get SharedMemory.SingletonKey.key with
    | None ->
        ()
    | Some protocols ->
        Protocols.remove_batch (Protocols.KeySet.singleton SharedMemory.SingletonKey.key);
        let protocols =
          Identifier.Set.Tree.to_list protocols
          |> Identifier.Set.Tree.of_list
        in
        Protocols.add SharedMemory.SingletonKey.key protocols
  end;
  Ast.SharedMemory.HandleKeys.normalize ();
  let handles = Ast.SharedMemory.HandleKeys.get () in
  File.Handle.Set.Tree.to_list handles
  |> SharedHandler.DependencyHandler.normalize
