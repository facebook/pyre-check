(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Pyre

open EnvironmentSharedMemory
open PostprocessSharedMemory


let populate
    (module Handler: Environment.Handler)
    ~configuration:{ Configuration.Analysis.debug; _ }
    sources =
  let resolution = TypeCheck.resolution (module Handler) () in
  List.iter ~f:(Environment.register_module (module Handler)) sources;

  let all_annotations =
    List.fold
      ~init:Type.Set.empty
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
    ~top:Type.Object
    all_annotations;
  TypeOrder.remove_extra_edges
    (module Handler.TypeOrderHandler)
    ~bottom:Type.Bottom
    ~top:Type.Object
    all_annotations;

  List.iter ~f:(Environment.register_functions (module Handler) resolution) sources;
  List.iter ~f:(Environment.register_globals (module Handler) resolution) sources;
  (* TODO(T30713406): Merge with class registration. *)
  List.iter ~f:Handler.refine_class_definition all_annotations;

  List.iter ~f:(Plugin.apply_to_environment (module Handler) resolution) sources;
  (* Calls to `attribute` might populate this cache, ensure it's cleared. *)
  Annotated.Class.Attribute.Cache.clear ()


let build
    ((module Handler: Environment.Handler) as handler)
    ~configuration
    ~stubs
    ~sources =
  Log.info "Building type environment...";
  (* This grabs all sources from shared memory. It is unavoidable: Environment
     must be built sequentially until we find a way to build the environment in
     parallel. *)
  let get_sources =
    List.fold
      ~init:[]
      ~f:(fun handles path ->
          match Ast.SharedMemory.Sources.get path with
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
  populate ~configuration handler (stubs @ sources);
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
  let function_definitions =
    FunctionDefinitions.get

  let class_definition =
    ClassDefinitions.get

  let register_protocol protocol =
    let protocols = Protocols.get "Protocols" |> Option.value ~default:[] in
    Protocols.remove_batch (Protocols.KeySet.singleton "Protocols");
    Protocols.add "Protocols" (protocol :: protocols)


  let protocols () =
    Protocols.get "Protocols"
    |> Option.value ~default:[]

  let register_module ~qualifier ~local_mode ~handle ~stub ~statements =
    let is_registered_empty_stub =
      Ast.SharedMemory.Modules.get ~qualifier
      >>| Module.empty_stub
      |> Option.value ~default:false
    in
    if not is_registered_empty_stub then
      begin
        Ast.SharedMemory.Modules.remove ~qualifiers:[qualifier];
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
      match get handle with
      | None -> add handle [key]
      | Some keys -> add handle (key :: keys)

    let add_function_key ~handle access =
      add_new_key ~handle ~key:access ~get:FunctionKeys.get ~add:FunctionKeys.add

    let add_class_key ~handle class_type =
      add_new_key ~handle ~key:class_type ~get:ClassKeys.get ~add:ClassKeys.add

    let add_alias_key ~handle alias =
      add_new_key ~handle ~key:alias ~get:AliasKeys.get ~add:AliasKeys.add

    let add_global_key ~handle global =
      add_new_key ~handle ~key:global ~get:GlobalKeys.get ~add:GlobalKeys.add

    let add_dependent_key ~handle dependent =
      add_new_key ~handle ~key:dependent ~get:DependentKeys.get ~add:DependentKeys.add

    let add_dependent ~handle dependent =
      add_dependent_key ~handle dependent;
      match Dependents.get dependent with
      | None -> Dependents.add dependent [handle]
      | Some dependencies -> Dependents.add dependent (handle :: dependencies)

    let get_function_keys ~handle = FunctionKeys.get handle |> Option.value ~default:[]
    let get_class_keys ~handle = ClassKeys.get handle |> Option.value ~default:[]
    let get_alias_keys ~handle = AliasKeys.get handle |> Option.value ~default:[]
    let get_global_keys ~handle = GlobalKeys.get handle |> Option.value ~default:[]
    let get_dependent_keys ~handle = DependentKeys.get handle |> Option.value ~default:[]

    let clear_keys_batch paths =
      FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list paths);
      ClassKeys.remove_batch (ClassKeys.KeySet.of_list paths);
      AliasKeys.remove_batch (AliasKeys.KeySet.of_list paths);
      GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list paths);
      DependentKeys.remove_batch (DependentKeys.KeySet.of_list paths)

    let dependents = Dependents.get
  end: Dependencies.Handler)

  module TypeOrderHandler = struct
    type ('key, 'value) lookup = {
      get: 'key -> 'value option;
      set: 'key -> 'value -> unit;
    }

    let edges () = {
      get = OrderEdges.get;
      set = (fun key value ->
          OrderEdges.remove_batch (OrderEdges.KeySet.singleton key);
          OrderEdges.add key value);
    }

    let backedges () = {
      get = OrderBackedges.get;
      set =
        (fun key value ->
           OrderBackedges.remove_batch (OrderBackedges.KeySet.singleton key);
           OrderBackedges.add key value);
    }

    let indices () = {
      get = OrderIndices.get;
      set =
        (fun key value ->
           OrderIndices.remove_batch (OrderIndices.KeySet.singleton key);
           OrderIndices.add key value);
    }

    let annotations () = {
      get = OrderAnnotations.get;
      set =
        (fun key value ->
           OrderAnnotations.remove_batch (OrderAnnotations.KeySet.singleton key);
           OrderAnnotations.add key value);
    }

    let find { get; _ } key = get key

    let find_unsafe { get; _ } key = Option.value_exn (get key)

    let contains { get; _ } key = Option.is_some (get key)

    let set { set; _ } ~key ~data =
      set key data

    let length _ =
      (OrderKeys.get "Order"
       >>| List.length)
      |> Option.value ~default:0

    let add_key key =
      match OrderKeys.get "Order" with
      | None -> OrderKeys.add "Order" [key]
      | Some keys ->
          OrderKeys.remove_batch (OrderKeys.KeySet.singleton "Order");
          OrderKeys.add "Order" (key :: keys)

    let keys () =
      Option.value ~default:[] (OrderKeys.get "Order")

    let show () =
      let keys =
        keys ()
        |> List.sort ~compare:Int.compare
      in
      let serialized_keys = List.to_string ~f:Int.to_string keys in
      let serialized_annotations =
        let serialize_annotation key =
          find (annotations ()) key
          >>| (fun annotation -> Format.asprintf "%d->%a\n" key Type.pp annotation)
        in
        List.filter_map ~f:serialize_annotation keys
        |> String.concat
      in
      let serialized_edges edges =
        let edges_of_key key =
          let show_successor { TypeOrder.Target.target = successor; _ } =
            Option.value_exn (find (annotations ()) successor)
            |> Type.show
          in
          Option.value ~default:[] (find edges key)
          |> List.to_string ~f:show_successor
        in
        List.to_string ~f:(fun key -> Format.asprintf "%d -> %s\n" key (edges_of_key key)) keys
      in
      Format.asprintf "Keys:\n%s\nAnnotations:\n%s\nEdges:\n%s\nBackedges:\n%s\n"
        serialized_keys
        serialized_annotations
        (serialized_edges (edges ()))
        (serialized_edges (backedges ()))
  end

  let register_definition
      ~handle
      ?name_override
      ({ Node.location; value = { Statement.Define.name; _ }; _ }) =
    let name = Option.value ~default:name name_override in
    DependencyHandler.add_function_key ~handle name;
    let annotation =
      Annotation.create_immutable ~global:true Type.Top
      |> Node.create ~location
    in
    Globals.remove_batch (Globals.KeySet.singleton name);
    Globals.add name annotation


  let refine_class_definition annotation =
    let open Statement in
    let refine
        { Resolution.class_definition = { Node.location; value = class_definition }; _ } =
      let successors = TypeOrder.successors (module TypeOrderHandler) annotation in
      let in_test =
        let is_unit_test
            { Resolution.class_definition = { Node.value = { Class.name; _ }; _ }; _ } =
          Access.equal name (Access.create "unittest.TestCase")
        in
        let successor_classes =
          successors
          |> List.filter_map ~f:ClassDefinitions.get
        in
        List.exists ~f:is_unit_test successor_classes
      in
      let explicit_attributes = Class.explicitly_assigned_attributes class_definition in
      let implicit_attributes = Class.implicit_attributes ~in_test class_definition in
      ClassDefinitions.remove_batch (ClassDefinitions.KeySet.singleton annotation);
      ClassDefinitions.add
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
      Expression.Access.pp dependency
      File.Handle.pp handle;
    DependencyHandler.add_dependent ~handle dependency


  let register_global ~handle ~access ~global =
    DependencyHandler.add_global_key ~handle access;
    Globals.remove_batch (Globals.KeySet.singleton access);
    Globals.add access global


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
            explicit_attributes = Statement.Access.SerializableMap.empty;
            implicit_attributes = Statement.Access.SerializableMap.empty;
            is_test = false;
          }
    in
    ClassDefinitions.remove_batch (ClassDefinitions.KeySet.singleton primitive);
    ClassDefinitions.add primitive definition

  let register_alias ~handle ~key ~data =
    DependencyHandler.add_alias_key ~handle key;
    Aliases.remove_batch (Aliases.KeySet.singleton key);
    Aliases.add key data

  let purge ?(debug = false) handles =
    let purge_dependents keys =
      let remove_path dependents =
        List.filter
          ~f:(fun dependent -> not (List.mem handles dependent ~equal:File.Handle.equal))
          dependents
      in
      List.iter
        ~f:(fun key -> Dependents.get key >>| remove_path >>| Dependents.add key |> ignore)
        keys;
      DependentKeys.remove_batch (DependentKeys.KeySet.of_list handles)
    in
    List.concat_map ~f:(fun handle -> DependencyHandler.get_function_keys ~handle) handles
    |> fun keys ->
    begin
      FunctionDefinitions.remove_batch (FunctionDefinitions.KeySet.of_list keys);
      (* We add a global name for each function definition as well. *)
      Globals.remove_batch (Globals.KeySet.of_list keys);
    end;

    (* Remove the connection to the parent (if any) for all
       classes defined in the updated paths. *)
    List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
    |> List.iter ~f:(TypeOrder.disconnect_successors (module TypeOrderHandler));

    List.concat_map ~f:(fun handle -> DependencyHandler.get_class_keys ~handle) handles
    |> fun keys -> ClassDefinitions.remove_batch (ClassDefinitions.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_alias_keys ~handle) handles
    |> fun keys -> Aliases.remove_batch (Aliases.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_global_keys ~handle) handles
    |> fun keys -> Globals.remove_batch (Globals.KeySet.of_list keys);

    List.concat_map ~f:(fun handle -> DependencyHandler.get_dependent_keys ~handle) handles
    |> purge_dependents;

    DependencyHandler.clear_keys_batch handles;
    List.map ~f:(fun handle -> Ast.Source.qualifier ~handle) handles
    |> fun qualifiers -> Ast.SharedMemory.Modules.remove ~qualifiers;

    if debug then
      (* If in debug mode, make sure the TypeOrder is still consistent. *)
      TypeOrder.check_integrity (module TypeOrderHandler)

  let local_mode path = ErrorModes.get path
end


(** First dumps environment to shared memory, then exposes through
    Environment_handler *)
let populate_shared_memory
    ~configuration
    ~stubs
    ~sources =
  let add_to_shared_memory
      {
        Environment.function_definitions;
        class_definitions;
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

      add_table OrderIndices.write_through indices;
      add_table OrderAnnotations.write_through annotations;
      OrderKeys.write_through "Order" (Hashtbl.keys annotations);
    in
    add_type_order order;

    add_table FunctionDefinitions.write_through function_definitions;
    add_table ClassDefinitions.write_through class_definitions;
    add_table Aliases.add aliases;
    add_table Globals.write_through globals;
    add_table Dependents.write_through dependents;
    add_table FunctionKeys.write_through (Hashtbl.map ~f:Hash_set.to_list function_keys);
    add_table ClassKeys.write_through (Hashtbl.map ~f:Hash_set.to_list class_keys);
    add_table AliasKeys.write_through (Hashtbl.map ~f:Hash_set.to_list alias_keys);
    add_table GlobalKeys.write_through (Hashtbl.map ~f:Hash_set.to_list global_keys);
    add_table DependentKeys.write_through (Hashtbl.map ~f:Hash_set.to_list dependent_keys);

    Protocols.write_through "Protocols" (Hash_set.to_list protocols);
    add_table
      (fun qualifier ast_module -> Ast.SharedMemory.Modules.add ~qualifier ~ast_module)
      modules;
    Statistics.performance ~name:"added environment to shared memory" ~timer ()
  in
  let environment = Environment.Builder.create () in
  let ((module InProcessHandler: Environment.Handler) as handler) =
    Environment.handler ~configuration environment
  in
  build handler ~configuration ~stubs ~sources;

  let resolution = TypeCheck.resolution (module InProcessHandler) () in
  Environment.infer_protocols ~handler:(module InProcessHandler) resolution ();

  TypeOrder.check_integrity (module InProcessHandler.TypeOrderHandler);
  add_to_shared_memory environment;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", EnvironmentSharedMemory.heap_size ()]
    ()
